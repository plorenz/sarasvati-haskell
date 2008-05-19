module Workflow.DatabaseWfEngine where


import Database.HDBC
import qualified Data.Map as Map
import Workflow.Engine
import Workflow.Util.DbUtil as DbUtil

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

createDatabaseWfProcess :: DatabaseWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfProcess a)
createDatabaseWfProcess (DatabaseWfEngine conn) graph nodeTypes userData =
    do wfRunId <- insertWfProcess conn graph
       return $ WfProcess wfRunId nodeTypes graph [] [] userData

createDatabaseNodeToken :: DatabaseWfEngine -> WfProcess a -> Node -> [ArcToken] -> IO NodeToken
createDatabaseNodeToken (DatabaseWfEngine conn) wfRun node arcTokens =
    do nextTokenId <- insertNodeToken conn wfRun node
       mapM (insertNodeTokenParent conn nextTokenId) arcTokens
       return $ NodeToken nextTokenId (nodeId node)

createDatabaseArcToken :: DatabaseWfEngine -> WfProcess a -> Arc -> NodeToken -> IO ArcToken
createDatabaseArcToken (DatabaseWfEngine conn) wfRun arc nodeToken =
    do nextTokenId <- insertArcToken conn wfRun arc nodeToken
       return $ ArcToken nextTokenId arc

completeDatabaseNodeToken :: DatabaseWfEngine -> NodeToken -> IO ()
completeDatabaseNodeToken (DatabaseWfEngine conn) token =
    do markNodeTokenComplete conn token

completeDatabaseArcToken :: DatabaseWfEngine -> ArcToken -> IO ()
completeDatabaseArcToken (DatabaseWfEngine conn) token =
    do markArcTokenComplete conn token

databaseTransactionBoundary :: DatabaseWfEngine -> IO ()
databaseTransactionBoundary (DatabaseWfEngine conn) =
    do commit conn