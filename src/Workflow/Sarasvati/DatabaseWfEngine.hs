{-
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
-}

module Workflow.Sarasvati.DatabaseWfEngine (DatabaseWfEngine(..)) where


import Data.Map as Map hiding (null, filter, map)
import Database.HDBC

import Workflow.Sarasvati.Engine
import Workflow.Sarasvati.DbUtil as DbUtil
import Workflow.Sarasvati.TokenUtil as TokenUtil

data DatabaseWfEngine = forall conn. IConnection conn => DatabaseWfEngine conn

instance WfEngine DatabaseWfEngine where
    createWfProcess     = createDatabaseWfProcess
    createNodeToken     = createDatabaseNodeToken
    createArcToken      = createDatabaseArcToken
    completeNodeToken   = completeDatabaseNodeToken
    completeArcToken    = completeDatabaseArcToken
    recordGuardResponse = recordDatabaseGuardResponse
    transactionBoundary = databaseTransactionBoundary
    setProcessAttr      = setDatabaseProcessAttr
    removeProcessAttr   = removeDatabaseProcessAttr
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

completeDatabaseArcToken :: DatabaseWfEngine -> ArcToken -> IO ()
completeDatabaseArcToken (DatabaseWfEngine conn) token =
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

completeDatabaseNodeToken :: DatabaseWfEngine -> NodeToken -> IO ()
completeDatabaseNodeToken (DatabaseWfEngine conn) token =
    do run conn sql [toSql (tokenId token)]
       return ()
    where
        sql = "update wf_node_token set complete_date = current_timestamp where id = ?"

recordDatabaseGuardResponse :: DatabaseWfEngine -> NodeToken -> GuardResponse -> IO ()
recordDatabaseGuardResponse (DatabaseWfEngine conn) token response =
    do run conn sql [toSql (tokenId token),
                     toSql (ordinal response::Int)]
       return ()
    where
        sql    = "update wf_node_token set guard_action = ? where id = ?"
        ordinal AcceptToken  = 0
        ordinal DiscardToken = 1
        ordinal (SkipNode _) = 2

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
        sql = "insert into wf_token_attr (attr_set_id, name, value) values (?, ?, ?)"

updateTokenAttr :: (IConnection conn) => conn -> TokenAttr -> IO ()
updateTokenAttr conn (TokenAttr nodeTokenId name value) =
    do run conn sql [toSql value,
                     toSql nodeTokenId,
                     toSql name]
       return ()
    where
        sql = "update wf_token_attr set value = ? where attr_set_id = ? and name = ?"

deleteTokenAttr :: (IConnection conn) => conn -> Int -> String -> IO ()
deleteTokenAttr conn nodeTokenId name =
    do run conn sql [toSql nodeTokenId,
                     toSql name]
       return ()
    where
        sql = "delete from wf_token_attr where attr_set_id = ? and name = ?"

insertProcessAttr :: (IConnection conn) => conn -> Int -> String -> String -> IO ()
insertProcessAttr conn processId name value =
    do run conn sql [toSql processId,
                     toSql name,
                     toSql value]
       return ()
    where
        sql = "insert into wf_process_attr (process_id, name, value) values (?, ?, ?)"

updateProcessAttr :: (IConnection conn) => conn -> Int -> String -> String -> IO ()
updateProcessAttr conn processId name value =
    do run conn sql [toSql value,
                     toSql processId,
                     toSql name]
       return ()
    where
        sql = "update wf_process_attr set value = ? where process_id = ? and name = ?"

deleteProcessAttr :: (IConnection conn) => conn -> Int -> String -> IO ()
deleteProcessAttr conn processId name =
    do run conn sql [toSql processId,
                     toSql name]
       return ()
    where
        sql = "delete from wf_process_attr where process_id = ? and name = ?"

createDatabaseWfProcess :: DatabaseWfEngine ->
                           WfGraph ->
                           Map.Map String (NodeType a) ->
                           Map.Map String (NodeToken -> WfProcess a -> IO Bool) ->
                           a ->
                           Map.Map String String ->
                           IO (WfProcess a)
createDatabaseWfProcess (DatabaseWfEngine conn) graph nodeTypes predicates userData attrs =
    do processId <- insertWfProcess conn graph
       mapM (uncurry (insertProcessAttr conn processId)) (Map.assocs attrs)
       return $ WfProcess processId nodeTypes graph [] [] attrs Map.empty predicates userData

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

setDatabaseProcessAttr :: DatabaseWfEngine-> WfProcess a -> String -> String -> IO (WfProcess a)
setDatabaseProcessAttr (DatabaseWfEngine conn) process key value
    | isUpdate    = do updateProcessAttr conn (processId process) key value
                       return newProcess
    | otherwise   = do insertProcessAttr conn (processId process) key value
                       return newProcess
    where
        newProcess  = process { attrMap = Map.insert key value (attrMap process) }
        isUpdate    = Map.member key (attrMap process)

removeDatabaseProcessAttr :: DatabaseWfEngine-> WfProcess a -> String -> IO (WfProcess a)
removeDatabaseProcessAttr (DatabaseWfEngine conn) process key
    | not hasKey  = return process
    | otherwise   = do deleteProcessAttr conn (processId process) key
                       return newProcess
    where
        newProcess  = process { attrMap = Map.delete key (attrMap process) }
        hasKey      = Map.member key (attrMap process)

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
