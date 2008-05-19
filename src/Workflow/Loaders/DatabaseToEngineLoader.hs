module Workflow.Loaders.DatabaseToEngineLoader where

import Database.HDBC
import Workflow.Engine
import Workflow.Loaders.LoadError
import qualified Data.Map as Map

loadGraph :: (IConnection conn) => conn -> String -> Map.Map String (conn -> Int -> IO NodeExtra) -> IO WfGraph
loadGraph conn name typeMap =
    do rows <- quickQuery conn sql [toSql name]
       if (null rows)
           then loadError $ "No graph found with name " ++ name
           else finishLoad conn (head rows) typeMap
    where
        sql = "select g.id, g.name, g.version from wf_graph g" ++
              " where g.name = ? and g.version in " ++
              "   (select max(g2.version) from wf_graph g2 where g2.name = g.name)"

finishLoad :: (IConnection conn) => conn -> [SqlValue] -> Map.Map String (conn -> Int -> IO NodeExtra) -> IO WfGraph
finishLoad conn row typeMap =
    do nodes <- loadNodes conn graphId typeMap
       arcs  <- loadArcs  conn graphId
       return $ graphFromArcs graphId graphName nodes arcs
    where
        graphId      = fromSql (row !! 0) :: Int
        graphName    = fromSql (row !! 1) :: String

loadNodes :: (IConnection conn) => conn -> Int -> Map.Map String (conn -> Int -> IO NodeExtra) -> IO [Node]
loadNodes conn graphId typeMap =
    do rows <- quickQuery conn sql [toSql graphId]
       mapM (rowToNode conn typeMap) rows
    where
        sql = "select r.id, n.id, n.name, n.type, n.is_join, r.instance, g.name, g.version " ++
              "  from wf_node_ref r " ++
              "  join wf_node n on (r.node_id = n.id) " ++
              "  join wf_graph g on (n.graph_id = g.id)" ++
              " where r.graph_id = ?"

rowToNode :: (IConnection conn) => conn -> Map.Map String (conn -> Int -> IO NodeExtra) -> [SqlValue] -> IO Node
rowToNode conn typeMap row =
    do nodeExtra <- nodeExtraIO
       return $ Node nodeRefId nodeType nodeName nodeSource isJoin nodeExtra
    where
        nodeRefId    = fromSql (row !! 0)
        nodeId       = fromSql (row !! 1) :: Int
        nodeName     = fromSql (row !! 2)
        nodeType     = fromSql (row !! 3)
        isJoin       = fromSql (row !! 4)
        nodeInstance = fromSql (row !! 5)
        graphName    = fromSql (row !! 6)
        graphVersion = fromSql (row !! 7)

        nodeSource   = NodeSource graphName graphVersion nodeInstance (nodeDepth nodeInstance)
        nodeExtraIO  = case (Map.member nodeType typeMap) of
                           True -> (typeMap Map.! nodeType) conn nodeId
                           False -> return NoNodeExtra

nodeDepth :: String -> Int
nodeDepth ""           = 0
nodeDepth instanceName = ((1+).length) $ filter (\c-> c == ':') instanceName

loadArcs :: (IConnection conn) => conn -> Int -> IO [Arc]
loadArcs conn graphId =
    do rows <- quickQuery conn sql [toSql graphId]
       return $ map (rowToArc) rows
    where
        sql = "select id, name, a_node_ref_id, z_node_ref_id " ++
              " from wf_arc where graph_id = ?"

rowToArc :: [SqlValue] -> Arc
rowToArc row = Arc arcId arcName aNodeId zNodeId
    where
        arcId   = fromSql (row !! 0)
        arcName = fromSql (row !! 1)
        aNodeId = fromSql (row !! 2)
        zNodeId = fromSql (row !! 3)