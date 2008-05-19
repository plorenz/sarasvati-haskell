module Workflow.DatabaseWfEngine where


import Database.HDBC
import qualified Data.Map as Map
import Workflow.Engine
import Workflow.Util.DbUtil as DbUtil
import Workflow.Util.TokenUtil as TokenUtil

data DatabaseWfEngine = forall conn. IConnection conn => DatabaseWfEngine conn

instance WfEngine DatabaseWfEngine where
    createWfProcess     = createDatabaseWfProcess
    createNodeToken     = createDatabaseNodeToken
    createArcToken      = createDatabaseArcToken
    completeNodeToken   = completeDatabaseNodeToken
    completeArcToken    = completeDatabaseArcToken
    transactionBoundary = databaseTransactionBoundary
    setTokenAttr        = setDatabaseTokenAttr
    removeTokenAttr     = removeDatabaseTokenAttr

insertWfProcess :: (IConnection conn) => conn -> WfGraph -> IO Int
insertWfProcess conn graph =
    do nextId <- DbUtil.nextSeqVal conn "wf_process_id_seq"
       run conn sql [toSql nextId,
                     toSql (graphId graph)]
       return nextId
    where
        sql = "insert into wf_process (id, graph_id) values ( ?, ? )"

insertArcToken :: (IConnection conn) => conn -> WfProcess a -> Arc -> NodeToken -> IO Int
insertArcToken conn wfProcess arc prevToken =
    do nextId <- DbUtil.nextSeqVal conn "wf_arc_token_id_seq"
       run conn sql [toSql nextId,
                     toSql (processId wfProcess),
                     toSql (arcId arc),
                     toSql (tokenId prevToken)]
       return nextId
    where
        sql = "insert into wf_arc_token (id, process_id, arc_id, parent_token_id) " ++
              " values ( ?, ?, ?, ? )"

markArcTokenComplete :: (IConnection conn) => conn -> ArcToken -> IO ()
markArcTokenComplete conn token =
    do run conn sql [toSql (tokenId token)]
       return ()
    where
        sql = "update wf_arc_token set complete_date = current_timestamp where id = ?"

insertNodeToken :: (IConnection conn) => conn -> WfProcess a -> Node -> IO Int
insertNodeToken conn wfProcess node  =
    do nextId <- DbUtil.nextSeqVal conn "wf_node_token_id_seq"
       run conn sql [toSql nextId,
                     toSql (processId wfProcess),
                     toSql (nodeId node)]
       return nextId
    where
        sql = "insert into wf_node_token (id, process_id, node_ref_id) values ( ?, ?, ? )"

insertNodeTokenParent :: (IConnection conn) => conn -> Int -> ArcToken -> IO ()
insertNodeTokenParent conn nodeTokenId arcToken =
    do run conn sql [toSql nodeTokenId,
                     toSql (tokenId arcToken)]
       return ()
    where
        sql = "insert into wf_node_token_parent (node_token_id, arc_token_id) values ( ?, ? )"

markNodeTokenComplete :: (IConnection conn) => conn -> NodeToken -> IO ()
markNodeTokenComplete conn token =
    do run conn sql [toSql (tokenId token)]
       return ()
    where
        sql = "update wf_node_token set complete_date = current_timestamp where id = ?"

updateNodeTokenAttrSet :: (IConnection conn) => conn -> Int -> Int -> IO ()
updateNodeTokenAttrSet conn tokenId attrSetId =
    do run conn sql [toSql attrSetId,
                     toSql tokenId]
       return ()
    where
        sql = "update wf_node_token set attr_set_id = ? where id = ?"

insertTokenAttr :: (IConnection conn) => conn -> TokenAttr -> IO ()
insertTokenAttr conn (TokenAttr nodeTokenId name value) =
    do run conn sql [toSql nodeTokenId,
                     toSql name,
                     toSql value]
       return ()
    where
        sql = "insert into wf_token_string_attr (attr_set_id, name, value) values (?, ?, ?)"

updateTokenAttr :: (IConnection conn) => conn -> TokenAttr -> IO ()
updateTokenAttr conn (TokenAttr nodeTokenId name value) =
    do run conn sql [toSql value,
                     toSql nodeTokenId,
                     toSql name]
       return ()
    where
        sql = "update wf_token_string_attr set value = ? where attr_set_id = ? and name = ?"

deleteTokenAttr :: (IConnection conn) => conn -> Int -> String -> IO ()
deleteTokenAttr conn nodeTokenId name =
    do run conn sql [toSql nodeTokenId,
                     toSql name]
       return ()
    where
        sql = "delete from wf_token_string_attr where attr_set_id = ? and name = ?"

createDatabaseWfProcess :: DatabaseWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfProcess a)
createDatabaseWfProcess (DatabaseWfEngine conn) graph nodeTypes userData =
    do wfRunId <- insertWfProcess conn graph
       return $ WfProcess wfRunId nodeTypes graph [] [] Map.empty userData

createDatabaseNodeToken :: DatabaseWfEngine -> WfProcess a -> Node -> [ArcToken] -> IO (WfProcess a, NodeToken)
createDatabaseNodeToken (DatabaseWfEngine conn) process node arcTokens =
    do nextTokenId <- insertNodeToken conn process node
       mapM (insertNodeTokenParent conn nextTokenId) arcTokens
       attrs <- databaseMergeTokenAttrs conn process nextTokenId arcTokens
       let newToken = NodeToken nextTokenId (nodeId node)
       return (replaceTokenAttrs process newToken attrs, newToken)

createDatabaseArcToken :: DatabaseWfEngine -> WfProcess a -> Arc -> NodeToken -> IO (WfProcess a, ArcToken)
createDatabaseArcToken (DatabaseWfEngine conn) process arc nodeToken =
    do nextTokenId <- insertArcToken conn process arc nodeToken
       return (process, ArcToken nextTokenId arc nodeToken)

completeDatabaseNodeToken :: DatabaseWfEngine -> NodeToken -> IO ()
completeDatabaseNodeToken (DatabaseWfEngine conn) token =
    do markNodeTokenComplete conn token

completeDatabaseArcToken :: DatabaseWfEngine -> ArcToken -> IO ()
completeDatabaseArcToken (DatabaseWfEngine conn) token =
    do markArcTokenComplete conn token

databaseTransactionBoundary :: DatabaseWfEngine -> IO ()
databaseTransactionBoundary (DatabaseWfEngine conn) =
    do commit conn

databaseMergeTokenAttrs :: (IConnection conn) => conn -> WfProcess a -> Int -> [ArcToken] -> IO [TokenAttr]
databaseMergeTokenAttrs _    _       _          []         = return []

databaseMergeTokenAttrs conn process newTokenId [arcToken]
    | null attrs = return attrs
    | otherwise  = do updateNodeTokenAttrSet conn newTokenId (tokenId (parentToken arcToken))
                      return attrs
    where
       attrs = parentAttrs process arcToken

databaseMergeTokenAttrs conn process newTokenId (x1:x2:xs)
    | null $ parentAttrs process x1 = databaseMergeTokenAttrs conn process newTokenId (x2:xs)
    | null $ parentAttrs process x2 = databaseMergeTokenAttrs conn process newTokenId (x1:xs)
    | otherwise                     = do updateNodeTokenAttrSet conn newTokenId newTokenId
                                         mapM (insertTokenAttr conn) mergedAttrs
                                         return mergedAttrs
    where
       mergedAttrs = map (updateAttrId newTokenId) $ TokenUtil.mergeTokenAttrs process (x1:x2:xs)

updateAttrId :: Int -> TokenAttr -> TokenAttr
updateAttrId newId tokenAttr = tokenAttr { attrSetId = newId }

updateTokenAttrId :: Int -> [TokenAttr] -> [TokenAttr]
updateTokenAttrId newSetId attrList = map (updateAttrId newSetId) attrList

setDatabaseTokenAttr :: DatabaseWfEngine-> WfProcess a -> NodeToken -> String -> String -> IO (WfProcess a)
setDatabaseTokenAttr (DatabaseWfEngine conn) process nodeToken key value
    | firstChange = do updateNodeTokenAttrSet conn (tokenId nodeToken) (tokenId nodeToken)
                       mapM (insertTokenAttr conn) finalAttrs
                       return newProcess
    | isUpdate    = do updateTokenAttr conn newAttr
                       return newProcess
    | otherwise   = do insertTokenAttr conn newAttr
                       return newProcess
    where
        newProcess  = replaceTokenAttrs process nodeToken finalAttrs
        finalAttrs  = case (firstChange) of
                          True  -> updateTokenAttrId (tokenId nodeToken) newAttrs
                          False -> newAttrs
        newAttrs    = TokenUtil.setOrReplaceTokenAttr attrs newAttr
        newAttr     = TokenAttr (tokenId nodeToken) key value
        attrs       = tokenAttrs process nodeToken
        isUpdate    = TokenUtil.nodeHasAttr process nodeToken key
        firstChange = null attrs || ((attrSetId.head) attrs) /= (tokenId nodeToken)

removeDatabaseTokenAttr :: DatabaseWfEngine-> WfProcess a -> NodeToken -> String -> IO (WfProcess a)
removeDatabaseTokenAttr (DatabaseWfEngine conn) process nodeToken key
    | not hasKey  = return process
    | firstChange = do updateNodeTokenAttrSet conn (tokenId nodeToken) (tokenId nodeToken)
                       mapM (insertTokenAttr conn) finalAttrs
                       return newProcess
    | otherwise   = do deleteTokenAttr conn (tokenId nodeToken) key
                       return newProcess
    where
        newProcess  = replaceTokenAttrs process nodeToken finalAttrs
        finalAttrs  = case (firstChange) of
                          True  -> updateTokenAttrId (tokenId nodeToken) newAttrs
                          False -> newAttrs
        newAttrs    = TokenUtil.removeTokenAttrFromList key attrs
        attrs       = tokenAttrs process nodeToken
        hasKey      = TokenUtil.nodeHasAttr process nodeToken key
        firstChange = null attrs || ((attrSetId.head) attrs) /= (tokenId nodeToken)
