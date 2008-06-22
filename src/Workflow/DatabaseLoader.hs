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

module Workflow.DatabaseLoader where

import Control.Monad
import qualified Data.Map as Map

import Database.HDBC
import qualified Workflow.Util.DbUtil as DbUtil

import Workflow.Engine
import Workflow.Error
import Workflow.Loader
import Workflow.Loaders.DatabaseToEngineLoader

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
        xmlExtra = xmlNodeExtra   xmlNode

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