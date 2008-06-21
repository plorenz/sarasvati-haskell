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

import qualified Data.Map as Map

import Database.HDBC
import qualified Workflow.Util.DbUtil as DbUtil

import Workflow.Engine
import Workflow.Error
import Workflow.Loader

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

getMaxGraphId :: (IConnection a) => a-> String -> IO Int
getMaxGraphId conn name =
    do rows <- quickQuery conn sql [toSql name]
       return $ (fromSql.head.head) rows
    where
        sql = "select coalesce( max(id), 0) from wf_graph where name = ?"

getNodeRefId :: (IConnection a) => a-> Int -> String -> String -> String -> IO Int
getNodeRefId conn newGraphId graphName nodeName instanceName =
    do graphId <- getMaxGraphId conn graphName
       rows <- quickQuery conn sql
                 [toSql newGraphId,
                  toSql instanceName,
                  toSql nodeName,
                  toSql graphId]
       case (null rows) of
           True -> wfError $ "Node with name " ++ nodeName ++ " for instance " ++ instanceName ++ " not found"
           False -> return $ (fromSql.head.head) rows
    where
        sql = "select ref.id from wf_node_ref ref " ++
              "               join wf_node node on (ref.node_id = node.id) " ++
              "  where ref.graph_id = ? and ref.instance = ? " ++
              "    and node.name = ? and node.graph_id = ? "
{-
getLoadArcs :: (IConnection conn) => conn -> String -> IO [LoadArc]
getLoadArcs conn graphName =
    do graphId <- getMaxGraphId conn graphName
       rows <- quickQuery conn sql [toSql graphId]
       return $ map (rowToLoadArc) rows
    where
        sql = "select name, a_node_ref_id, z_node_ref_id from wf_arc where graph_id = ?"

rowToLoadArc :: [SqlValue] -> LoadArc
rowToLoadArc row = LoadArc name refA refZ
    where
        name = fromSql $ row !! 0
        refA = fromSql $ row !! 1
        refZ = fromSql $ row !! 2
-}

data DbLoader = forall conn . IConnection conn => DbLoader conn (Map.Map String (DbLoader -> Int -> XmlNode -> IO ()))

instance Loader (DbLoader) where
    createWorkflow = createDbWorkflow
    createNode     = createDbNode
    createArc      = createDbArc
    importInstance = importDbInstance

createDbWorkflow :: DbLoader -> XmlWorkflow -> IO Int
createDbWorkflow (DbLoader conn _) xmlWf = insertNewGraph conn (xmlWfName xmlWf)

createDbNode :: DbLoader -> Int -> XmlNode -> IO Node
createDbNode loader@(DbLoader conn funcMap) graphId xmlNode =
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
createDbArc (DbLoader conn _) graphId arcName startNode endNode =
    do arcId <-insertArc conn graphId (nodeId startNode) (nodeId endNode) arcName
       return $ Arc arcId arcName (nodeId startNode) (nodeId endNode)

importDbInstance :: DbLoader -> Int -> String -> IO ([Node],[Arc])
importDbInstance (DbLoader conn funcMap) graphId graphName = return ([],[])