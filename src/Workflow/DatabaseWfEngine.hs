module Workflow.DatabaseWfEngine where


import Database.HDBC
import qualified Data.Map as Map
import Workflow.Engine
import Workflow.Util.DbUtil as DbUtil

data DatabaseWfEngine =
    DatabaseWfEngine {
       engineConn :: ConnWrapper
    }

instance WfEngine DatabaseWfEngine where
    createWfRun     = createDatabaseWfRun
    createNodeToken = createDatabaseNodeToken
    createArcToken  = createDatabaseArcToken

insertWfRun :: (IConnection conn) => conn -> WfGraph -> IO Int
insertWfRun conn graph =
    do nextId <- DbUtil.nextSeqVal conn "wf_run_id_seq"
       run conn sql [toSql nextId,
                     toSql (graphId graph)]
       return nextId
    where
        sql = "insert into wf_run (id, graph_id) values ( ?, ? )"

insertArcToken :: (IConnection conn) => conn -> WfRun a -> Arc -> NodeToken -> IO Int
insertArcToken conn wfRun arc prevToken =
    do nextId <- DbUtil.nextSeqVal conn "wf_arc_token_id_seq"
       run conn sql [toSql nextId,
                     toSql (runId wfRun),
                     toSql (arcId arc),
                     toSql (tokenId prevToken)]
       return nextId
    where
        sql = "insert into wf_arc_token (id, run_id, arc_id, prev_token_id) " ++
              " values ( ?, ?, ?, ? )"


insertNodeToken :: (IConnection conn) => conn -> WfRun a -> Node -> IO Int
insertNodeToken conn wfRun node  =
    do nextId <- DbUtil.nextSeqVal conn "wf_node_token_id_seq"
       run conn sql [toSql nextId,
                     toSql (runId wfRun),
                     toSql (nodeId node)]
       return nextId
    where
        sql = "insert into wf_node_token (id, run_id, node_ref_id) values ( ?, ?, ? )"

insertNodeTokenParent :: (IConnection conn) => conn -> Int -> ArcToken -> IO ()
insertNodeTokenParent conn nodeTokenId arcToken =
    do run conn sql [toSql nodeTokenId,
                     toSql (tokenId arcToken)]
       return ()
    where
        sql = "insert into wf_node_token_parent (node_token_id, arc_token_id) values ( ?, ? )"


createDatabaseWfRun :: DatabaseWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfRun a)
createDatabaseWfRun engine graph nodeTypes userData =
    do wfRunId <- insertWfRun conn graph
       return $ WfRun wfRunId nodeTypes graph [] [] userData
    where
        conn = engineConn engine

createDatabaseNodeToken :: DatabaseWfEngine -> WfRun a -> Node -> [ArcToken] -> IO NodeToken
createDatabaseNodeToken engine wfRun node arcTokens =
    do nextTokenId <- insertNodeToken conn wfRun node
       mapM (insertNodeTokenParent conn nextTokenId) arcTokens
       return $ NodeToken nextTokenId (nodeId node)
    where
        conn = engineConn engine


createDatabaseArcToken :: DatabaseWfEngine -> WfRun a -> Arc -> NodeToken -> IO ArcToken
createDatabaseArcToken engine wfRun arc nodeToken =
    do nextTokenId <- insertArcToken conn wfRun arc nodeToken
       return $ ArcToken nextTokenId arc
    where
        conn = engineConn engine