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
insertTokenAttr conn (TokenAttr nodeTokenId name (TokenAttrString value)) =
    do run conn sql [toSql nodeTokenId,
                     toSql name,
                     toSql value]
       return ()
    where
        sql = "insert into wf_token_string_attr (attr_set_id, name, value) values (?, ?, ?)"

createDatabaseWfProcess :: DatabaseWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfProcess a)
createDatabaseWfProcess (DatabaseWfEngine conn) graph nodeTypes userData =
    do wfRunId <- insertWfProcess conn graph
       return $ WfProcess wfRunId nodeTypes graph [] [] userData

createDatabaseNodeToken :: DatabaseWfEngine -> WfProcess a -> Node -> [ArcToken] -> IO NodeToken
createDatabaseNodeToken (DatabaseWfEngine conn) wfRun node arcTokens =
    do nextTokenId <- insertNodeToken conn wfRun node
       mapM (insertNodeTokenParent conn nextTokenId) arcTokens
       attrs <- tokenAttrs conn nextTokenId arcTokens
       return $ NodeToken nextTokenId (nodeId node) attrs

createDatabaseArcToken :: DatabaseWfEngine -> WfProcess a -> Arc -> NodeToken -> IO ArcToken
createDatabaseArcToken (DatabaseWfEngine conn) wfRun arc nodeToken =
    do nextTokenId <- insertArcToken conn wfRun arc nodeToken
       return $ ArcToken nextTokenId arc nodeToken

completeDatabaseNodeToken :: DatabaseWfEngine -> NodeToken -> IO ()
completeDatabaseNodeToken (DatabaseWfEngine conn) token =
    do markNodeTokenComplete conn token

completeDatabaseArcToken :: DatabaseWfEngine -> ArcToken -> IO ()
completeDatabaseArcToken (DatabaseWfEngine conn) token =
    do markArcTokenComplete conn token

databaseTransactionBoundary :: DatabaseWfEngine -> IO ()
databaseTransactionBoundary (DatabaseWfEngine conn) =
    do commit conn

tokenAttrs :: (IConnection conn) => conn -> Int -> [ArcToken] -> IO [TokenAttr]
tokenAttrs _    _          []         = return []

tokenAttrs conn newTokenId [arcToken]
    | null attrs = return attrs
    | otherwise  = do updateNodeTokenAttrSet conn newTokenId (tokenId (parentToken arcToken))
                      return attrs
    where
       attrs = parentAttr arcToken

tokenAttrs conn newTokenId (x1:x2:xs)
    | null $ parentAttr x1 = tokenAttrs conn newTokenId (x2:xs)
    | null $ parentAttr x2 = tokenAttrs conn newTokenId (x1:xs)
    | otherwise            = do updateNodeTokenAttrSet conn newTokenId newTokenId
                                mapM (insertTokenAttr conn) mergedAttrs
                                return mergedAttrs
    where
       mergedAttrs = map (updateAttrId newTokenId) $ TokenUtil.mergeTokenAttrs (x1:x2:xs)

updateAttrId :: Int -> TokenAttr -> TokenAttr
updateAttrId newId tokenAttr = tokenAttr { tokenAttrId = newId }