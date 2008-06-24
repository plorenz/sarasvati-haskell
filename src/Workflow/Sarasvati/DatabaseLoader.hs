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

module Workflow.Sarasvati.DatabaseLoader (loadLatestGraph, loadGraph, DbLoader(..)) where

import Control.Monad
import Data.Map as Map hiding (null, filter, map)

import Database.HDBC

import Workflow.Sarasvati.DbUtil as DbUtil
import Workflow.Sarasvati.Engine
import Workflow.Sarasvati.Error
import Workflow.Sarasvati.Loader

---------------------------------------------------------------------------------------------------
--               Database to Engine Loading                                                      --
---------------------------------------------------------------------------------------------------

-- | Loads latest (with the highest version number) 'WfGraph' from the database with the given name.
--   Take a 'Map' of 'String' types to loaders for those types. Loading of the basic 'Node' information
--   is handled automatically. However, if a given 'Node' type has extra information in another table
--   which should be loaded into the 'NodeExtra', a loader function can be specified. It will be called
--   with the node id. If no function is specified for a given type, it's 'nodeExtra' will be set to
--   'NoNodeExtra'.
--
--  Parameters:
--
--    * conn - The HDBC database connection to use to connect to the database
--
--    * name - The name of the 'WfGraph' to load
--
--    * typeMap - 'Map' of type name to function for loading 'NodeExtra'
--
--  If a database error is encounered, a 'SqlError' will be thrown. If a loading error occurs, due
--  to missing or inconsistent data, a 'WfError'will be thrown.

loadLatestGraph :: (IConnection conn) => conn -> String -> Map.Map String (conn -> Int -> IO NodeExtra) -> IO WfGraph
loadLatestGraph conn name typeMap =
    do rows <- quickQuery conn sql [toSql name]
       if (null rows)
           then wfError $ "No graph found with name " ++ name
           else finishLoad conn (head rows) typeMap
    where
        sql = "select g.id, g.name, g.version from wf_graph g" ++
              " where g.name = ? and g.version in " ++
              "   (select max(g2.version) from wf_graph g2 where g2.name = g.name)"

-- | Like 'loadLatestGraph', except the specific version number of the 'WfGraph' to be loaded is given,
--   rather than assuming the newest version is to be loaded.

loadGraph :: (IConnection conn) => conn -> String -> Int -> Map.Map String (conn -> Int -> IO NodeExtra) -> IO WfGraph
loadGraph conn name version typeMap =
    do rows <- quickQuery conn sql [toSql name, toSql version]
       if (null rows)
           then wfError $ "No graph found with name " ++ name ++ " and version " ++ (show version)
           else finishLoad conn (head rows) typeMap
    where
        sql = "select g.id, g.name, g.version from wf_graph g" ++
              " where g.name = ? and g.version = ?"

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
        sql = "select r.id, n.id, n.name, n.type, n.is_join, n.is_start, coalesce( n.guard, '' ), " ++
              "r.graph_id = n.graph_id as is_top_level" ++
              "  from wf_node_ref r " ++
              "  join wf_node n on (r.node_id = n.id) " ++
              "  join wf_graph g on (n.graph_id = g.id)" ++
              " where r.graph_id = ?"

rowToNode :: (IConnection conn) => conn -> Map.Map String (conn -> Int -> IO NodeExtra) -> [SqlValue] -> IO Node
rowToNode conn typeMap row =
    do nodeExtra <- nodeExtraIO
       return $ Node nodeRefId nodeType nodeName isJoin isStart (not isTopLevel) guard nodeExtra
    where
        nodeRefId    = fromSql (row !! 0)
        nodeId       = fromSql (row !! 1) :: Int
        nodeName     = fromSql (row !! 2)
        nodeType     = fromSql (row !! 3)
        isJoin       = "Y" == fromSql (row !! 4)
        isStart      = ("Y" == fromSql (row !! 5)) && isTopLevel
        guard        = fromSql (row !! 6)
        isTopLevel   = fromSql (row !! 7)

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

--------------------------------------------------------------------------------
-- Database Functions
--------------------------------------------------------------------------------

insertNewGraph :: (IConnection a) => a -> String -> IO Int
insertNewGraph conn name =
    do maxVersion <- getMaxGraphVersion conn name
       putStrLn $ "Current version of " ++ name ++ " is " ++ (show maxVersion)
       nextId <- DbUtil.nextSeqVal conn "wf_graph_id_seq"
       run conn sql [toSql nextId,
                     toSql name,
                     toSql (maxVersion + 1)]
       putStrLn $ "Inserted version " ++ show (maxVersion + 1) ++ " of " ++ name ++ " with id " ++ (show nextId)
       return nextId
    where
        sql = "insert into wf_graph (id, name, version) values ( ?, ?, ? )"

insertNodeWithRef :: (IConnection conn) => conn -> Int -> String -> Bool -> Bool -> String -> String -> IO (Int,Int)
insertNodeWithRef conn graphId nodeName isJoin isStart nodeType guard =
    do nextNodeId <- DbUtil.nextSeqVal conn "wf_node_id_seq"
       run conn nodeSql
                 [toSql nextNodeId,
                  toSql graphId,
                  toSql nodeName,
                  toSql $ if isJoin then "Y" else "N",
                  toSql $ if isStart then "Y" else "N",
                  toSql nodeType,
                  toSql guard]
       nextNodeRefId <- DbUtil.nextSeqVal conn "wf_node_ref_id_seq"
       run conn nodeRefSql
                 [toSql nextNodeRefId,
                  toSql nextNodeId,
                  toSql graphId]
       return (nextNodeId, nextNodeRefId)
    where
        nodeSql    = "insert into wf_node (id, graph_id, name, is_join, is_start, type, guard) " ++
                     " values ( ?, ?, ?, ?, ?, ?, ? )"
        nodeRefSql = "insert into wf_node_ref (id, node_id, graph_id, instance) " ++
                     " values (?, ?, ?, '' )"

insertNodeRef :: (IConnection conn) => conn -> Int -> Int -> String -> IO Int
insertNodeRef conn graphId copyRefId instanceName =
    do putStrLn $ "Copying ref: " ++ (show copyRefId) ++ " using instance name: " ++ instanceName ++ " in graph: " ++ (show graphId)
       nextNodeRefId <- DbUtil.nextSeqVal conn "wf_node_ref_id_seq"
       run conn nodeRefSql
                 [toSql nextNodeRefId,
                  toSql graphId,
                  toSql instanceName,
                  toSql instanceName,
                  toSql copyRefId]
       return nextNodeRefId
    where
        nodeRefSql = "insert into wf_node_ref (id, node_id, graph_id, instance) " ++
                     " select ?, node_id, ?, " ++
                     "        CASE WHEN instance = '' then ? ELSE ? || ':' || instance END " ++
                     " from wf_node_ref where id = ? "

insertArc :: (IConnection conn) => conn -> Int -> Int -> Int -> String -> IO Int
insertArc conn graphId startNode endNode arcName =
    do nextArcId <- DbUtil.nextSeqVal conn "wf_arc_id_seq"
       run conn sql
                 [toSql nextArcId,
                  toSql graphId,
                  toSql startNode,
                  toSql endNode,
                  toSql arcName]
       return nextArcId
    where
        sql = "insert into wf_arc (id, graph_id, a_node_ref_id, z_node_ref_id, name ) " ++
              " values ( ?, ?, ?, ?, ? ) "

getMaxGraphVersion :: (IConnection a) => a-> String -> IO Int
getMaxGraphVersion conn name =
    do rows <- quickQuery conn sql [toSql name]
       return $ (fromSql.head.head) rows
    where
        sql = "select coalesce( max(version), 0) from wf_graph where name = ?"


data DbLoader =
  forall conn . IConnection conn =>
    DbLoader conn  (Map.Map String (DbLoader -> Int -> XmlNode -> IO ()))
                   (Map.Map String (conn -> Int -> IO NodeExtra))

instance Loader (DbLoader) where
    createWorkflow = createDbWorkflow
    createNode     = createDbNode
    createArc      = createDbArc
    importInstance = importDbInstance

createDbWorkflow :: DbLoader -> XmlWorkflow -> IO Int
createDbWorkflow (DbLoader conn _ _) xmlWf = insertNewGraph conn (xmlWfName xmlWf)

createDbNode :: DbLoader -> Int -> XmlNode -> IO Node
createDbNode loader@(DbLoader conn funcMap _) graphId xmlNode =
    do (nodeId, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin isStart nodeType guard
       case (Map.member nodeType funcMap) of
           True  -> (funcMap Map.! nodeType) loader nodeId xmlNode
           False -> return ()
       return $ xmlNodeToNode nodeRefId xmlNode
    where
        nodeName = xmlNodeName    xmlNode
        isJoin   = xmlNodeIsJoin  xmlNode
        isStart  = xmlNodeIsStart xmlNode
        nodeType = xmlNodeType    xmlNode
        guard    = xmlNodeGuard   xmlNode

createDbArc :: DbLoader -> Int -> String -> Node -> Node -> IO Arc
createDbArc (DbLoader conn _ _) graphId arcName startNode endNode =
    do arcId <-insertArc conn graphId (nodeId startNode) (nodeId endNode) arcName
       return $ Arc arcId arcName (nodeId startNode) (nodeId endNode)

importDbInstance :: DbLoader -> Int -> String -> String -> IO ([Node],[Arc])
importDbInstance loader@(DbLoader conn _ funcMap) graphId graphName instanceName =
    do wfGraph <- loadLatestGraph conn graphName funcMap
       nodeMap <- foldM (importNode loader graphId instanceName) Map.empty (Map.elems $ graphNodes wfGraph)
       arcs    <- mapM (importArcs loader graphId nodeMap) ((concat.Map.elems) $ graphInputArcs wfGraph)
       return (Map.elems nodeMap,arcs)

importNode :: DbLoader -> Int -> String -> Map.Map Int Node -> Node -> IO (Map.Map Int Node)
importNode (DbLoader conn _ _) graphId instanceName nodeMap node =
    do newNodeId <- insertNodeRef conn graphId (nodeId node) instanceName
       return $ Map.insert (nodeId node) (node {nodeId = newNodeId}) nodeMap

importArcs :: DbLoader -> Int -> Map.Map Int Node -> Arc -> IO Arc
importArcs (DbLoader conn _ _) graphId nodeMap arc
    | not (Map.member (startNodeId arc) nodeMap) = error $ "When importing arc, new node not found"
    | not (Map.member (endNodeId arc) nodeMap)   = error $ "When importing arc, new node not found"
    | otherwise = do newArcId <- insertArc conn graphId newStartNodeId newEndNodeId (arcName arc)
                     return $ Arc newArcId (arcName arc) newStartNodeId newEndNodeId
    where
        newStartNodeId = nodeId $ nodeMap Map.! (startNodeId arc)
        newEndNodeId   = nodeId $ nodeMap Map.! (endNodeId   arc)