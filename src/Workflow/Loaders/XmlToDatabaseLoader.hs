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


module Workflow.Loaders.XmlToDatabaseLoader where

import Control.Monad
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.Map as Map
import Workflow.Util.XmlUtil as XmlUtil
import qualified Workflow.Util.DbUtil as DbUtil
import qualified Workflow.Util.ListUtil as ListUtil
import Workflow.Loaders.WfLoad

--------------------------------------------------------------------------------
-- Data Type Definitions
--------------------------------------------------------------------------------

-- The NodeArcs gives both incoming and outgoing nodes. However, when loading a graph, the arcs are
-- defined only one way (for simplicity and correctness). The LoadNode maps to what a workflow graph
-- definition node mostly likely looks like.

data LoadNode =
    LoadNode {
        nodeId       :: Int,
        nodeName     :: String,
        arcs         :: [(String,Int)],
        arcRefs      :: [(String,String)],
        externalArcs :: [ExternalArc]
    }
 deriving (Show)


data LoadArc =
    LoadArc {
        loadArcName  :: String,
        loadArcRefA  :: Int,
        loadArcRefZ  :: Int
    }
  deriving (Show)

--------------------------------------------------------------------------------
-- XML Functions
--------------------------------------------------------------------------------

loadDocFromFile :: FilePath -> IO (Either String Document)
loadDocFromFile filename =
    do fileContents <- readFile filename
       return $ xmlParse' filename fileContents

readArcs :: Element -> [(String, String)]
readArcs element = map (readArc) arcChildren
    where
        arcChildren   = XmlUtil.toElem $ ((tag "arc") `o` children) (CElem element)
        readArc e     = (readOptionalAttr e "name" "", readRequiredAttr e "to")

readExternalArcs :: Element -> [ExternalArc]
readExternalArcs element = map (readExternalArcFromElem) childElem
    where
        childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

readExternalArcFromElem :: Element -> ExternalArc
readExternalArcFromElem e = ExternalArc nodeId workflowId instanceId arcName arcType
    where
        workflowId = readRequiredAttr e "workflow"
        instanceId = readRequiredAttr e "instance"
        nodeId     = readRequiredAttr e "nodeId"
        arcName    = readOptionalAttr e "name" ""
        arcType    = case (readRequiredAttr e "type") of
                         "in" -> InArc
                         _    -> OutArc

graphName :: Document -> String
graphName doc = readRequiredAttr (rootElement doc) "id"

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

insertNodeWithRef :: (IConnection conn) => conn -> Int -> String -> Bool -> String -> String -> IO (Int,Int)
insertNodeWithRef conn graphId nodeName isJoin nodeType guard =
    do nextNodeId <- DbUtil.nextSeqVal conn "wf_node_id_seq"
       run conn nodeSql
                 [toSql nextNodeId,
                  toSql graphId,
                  toSql nodeName,
                  toSql $ if isJoin then "Y" else "N",
                  toSql nodeType,
                  toSql guard]
       nextNodeRefId <- DbUtil.nextSeqVal conn "wf_node_ref_id_seq"
       run conn nodeRefSql
                 [toSql nextNodeRefId,
                  toSql nextNodeId,
                  toSql graphId]
       return (nextNodeId, nextNodeRefId)
    where
        nodeSql    = "insert into wf_node (id, graph_id, name, is_join, type, guard) " ++
                     " values ( ?, ?, ?, ?, ?, ? )"
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

insertArc :: (IConnection conn) => conn -> Int -> Int -> Int -> String -> IO ()
insertArc conn graphId startNode endNode arcName =
    do nextArcId <- DbUtil.nextSeqVal conn "wf_arc_id_seq"
       run conn sql
                 [toSql nextArcId,
                  toSql graphId,
                  toSql startNode,
                  toSql endNode,
                  toSql arcName]
       return ()
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
           True -> wfLoadError $ "Node with name " ++ nodeName ++ " for instance " ++ instanceName ++ " not found"
           False -> return $ (fromSql.head.head) rows
    where
        sql = "select ref.id from wf_node_ref ref " ++
              "               join wf_node node on (ref.node_id = node.id) " ++
              "  where ref.graph_id = ? and ref.instance = ? " ++
              "    and node.name = ? and node.graph_id = ? "

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

--------------------------------------------------------------------------------
-- Load Functions
--------------------------------------------------------------------------------

processDoc ::
    (IConnection conn) =>
        Document ->
        Map.Map String (Element -> conn -> Int -> IO (Int, String)) ->
        conn ->
        IO Int
processDoc doc funcMap conn =
    do graphId   <- insertNewGraph conn (graphName doc)
       loadNodes <- mapM (processChildNode graphId funcMap conn) childNodes
       let resolvedLoadNodes = resolveArcs loadNodes
       mapM (processArcs conn graphId) resolvedLoadNodes
       processAllExternals conn graphId resolvedLoadNodes
       return graphId
    where
        childNodes = getChildren (rootElement doc)

processAllExternals :: (IConnection conn) => conn -> Int -> [LoadNode] -> IO (Map.Map String Bool)
processAllExternals conn graphId nodes =
    foldM (processNodeExternals conn graphId) Map.empty nodes

processNodeExternals :: (IConnection conn) => conn -> Int -> Map.Map String Bool -> LoadNode -> IO (Map.Map String Bool)
processNodeExternals conn graphId instanceMap node =
    foldM (processExternal conn graphId node) instanceMap (externalArcs node)

processExternal :: (IConnection conn) => conn -> Int -> LoadNode -> Map.Map String Bool -> ExternalArc -> IO (Map.Map String Bool)
processExternal conn graphId node instanceMap extArc =
    do putStrLn $ "Loading external: " ++ (show extArc)
       newInstanceMap <- ensureInstanceLoaded conn instanceMap graphId extArc
       targetNodeId <- getNodeRefId conn graphId wfName targetName instanceName
       case (arcType extArc) of
           InArc  -> insertArc conn graphId targetNodeId (nodeId node) (extArcName extArc)
           OutArc -> insertArc conn graphId (nodeId node) targetNodeId (extArcName extArc)
       return newInstanceMap
    where
       wfName       = targetWf extArc
       instanceName = targetInstance extArc
       targetName   = targetNodeName extArc

ensureInstanceLoaded :: (IConnection conn) => conn -> Map.Map String Bool -> Int -> ExternalArc -> IO (Map.Map String Bool)
ensureInstanceLoaded conn instanceMap graphId extArc =
    case (Map.member instanceName instanceMap) of
        True  -> return instanceMap
        False -> importInstance conn instanceMap graphId extArc
    where
        instanceName = targetInstance extArc

importInstance :: (IConnection conn) => conn -> Map.Map String Bool -> Int -> ExternalArc -> IO (Map.Map String Bool)
importInstance conn instanceMap graphId extArc =
    do putStrLn $ "Importing: " ++ wfName
       loadArcs <- getLoadArcs conn wfName
       foldM (importArc conn graphId instanceName) Map.empty loadArcs
       return $ Map.insert instanceName True instanceMap
    where
       wfName = targetWf extArc
       instanceName = targetInstance extArc

importArc :: (IConnection conn) => conn -> Int -> String -> Map.Map Int Int -> LoadArc -> IO (Map.Map Int Int)
importArc conn graphId instanceName refMap (LoadArc arcName refA refZ) =
    do refMapA <- importNode conn graphId instanceName refA refMap
       refMapZ <- importNode conn graphId instanceName refZ refMapA
       insertArc conn graphId (refMapZ Map.! refA) (refMapZ Map.! refZ) arcName
       return refMapZ

importNode :: (IConnection conn) => conn -> Int -> String -> Int -> Map.Map Int Int -> IO (Map.Map Int Int)
importNode conn graphId instanceName refId refMap =
    case (Map.member refId refMap) of
        True  -> return refMap
        False -> do newRefId <- insertNodeRef conn graphId refId instanceName
                    return $ Map.insert refId newRefId refMap

--------------------------------------------------------------------------------
-- Load Functions - Arc related
--------------------------------------------------------------------------------

processArcs :: (IConnection conn) => conn -> Int -> LoadNode -> IO [()]
processArcs conn graphId loadNode = mapM (processArc) (arcs loadNode)
    where
        processArc arc = insertArc conn graphId startId (snd arc) (fst arc)
        startId = nodeId loadNode

resolveArcs :: [LoadNode] -> [LoadNode]
resolveArcs loadNodes = map (resolveArcs') loadNodes
    where
        resolveArcs' loadNode = loadNode {arcs = map (resolveArc loadNodes) (arcRefs loadNode) }

resolveArc :: [LoadNode] -> (String,String) -> (String, Int)
resolveArc loadNodes arc
    | noTarget      = wfLoadError $ "No node with name " ++ targetName ++
                                  " found while looking for arc endpoint"
    | toManyTargets = wfLoadError $ "Too many nodes with name " ++ targetName ++
                                  " found while looking for arc endpoint"
    | otherwise     = (arcName, (nodeId.head) targetNodes)
    where
        arcName       = fst arc
        targetName    = snd arc
        targetNodes   = filter (\n->nodeName n == targetName) loadNodes
        noTarget      = null targetNodes
        toManyTargets = length targetNodes > 1

--------------------------------------------------------------------------------
-- Load Functions - Node related
--------------------------------------------------------------------------------

processChildNode ::
    (IConnection conn) =>
        Int ->
        Map.Map String (Element -> conn -> Int -> IO (Int, String)) ->
        conn ->
        Element ->
        IO LoadNode
processChildNode graphId funcMap conn e =
    do (nodeId, nodeName) <- nodeFunction e conn graphId
       return $ LoadNode nodeId nodeName [] arcs extArcs
    where
        elemName     = case (e) of (Elem name _ _ ) -> name
        nodeFunction = case (Map.member elemName funcMap) of
                           True  -> funcMap Map.! elemName
                           False -> processUnknown
        arcs         = readArcs e
        extArcs      = readExternalArcs e

processStart :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
processStart element conn graphId =
    do (_, nodeRefId) <- insertNodeWithRef conn graphId "start" False "start" guard
       return (nodeRefId, "start")
    where
        guard = ListUtil.trim $ readText element "guard"

processNode :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
processNode element conn graphId =
   do (_, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin "node" guard
      return (nodeRefId, nodeName)
    where
        nodeName = readRequiredAttr element "name"
        isJoin = case (readOptionalAttr element "isJoin" "false" ) of
                     "false" -> False
                     _       -> True
        guard = ListUtil.trim $ readText element "guard"

processUnknown :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
processUnknown element conn graphId =
   do (_, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin nodeType guard
      return (nodeRefId, nodeName)
    where
        nodeType = elementName element
        nodeName = readRequiredAttr element "name"
        isJoin   = case (readOptionalAttr element "isJoin" "false" ) of
                       "false" -> False
                       _       -> True
        guard = ListUtil.trim $ readText element "guard"

loadFromXmlToDB :: (IConnection conn) =>
     FilePath ->
     conn ->
     Map.Map Name (Element -> conn -> Int -> IO (Int,String)) ->
     IO (Either String Int)
loadFromXmlToDB filename conn funcMap =
    do maybeDoc <- loadDocFromFile filename
       case maybeDoc of
           Left msg  -> return $ Left msg
           Right doc -> do graphId <- withTransaction conn (processDoc doc funcMap)
                           return $ Right graphId

initialLoaderMap :: (IConnection conn) => Map.Map String (Element -> conn -> Int -> IO (Int, String))
initialLoaderMap = Map.fromList [ ("start", processStart),
                                  ("node",  processNode) ]

loaderMapWith ::
    (IConnection conn) =>
        [(String, (Element -> conn -> Int -> IO (Int, String)))] ->
        Map.Map String (Element -> conn -> Int -> IO (Int, String))
loaderMapWith list = addToMap list initialLoaderMap
   where
       addToMap []     map = map
       addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map

loadWorkflow :: (IConnection conn) => String -> conn -> Map.Map String (Element -> conn -> Int -> IO (Int, String)) -> IO (Either String Int)
loadWorkflow filename conn funcMap = do handleAll (loadFromXmlToDB filename conn funcMap)
    where
        handleAll = (handleSql handleDbError).(handleWfLoad handleLoadError).(handleXml handleXmlError)

handleLoadError :: WfLoadError -> IO (Either String a)
handleLoadError (WfLoadError msg) =
    do putStrLn msg
       return $ Left msg

handleDbError :: SqlError -> IO (Either String a)
handleDbError sqlError =
    do putStrLn msg
       return $ Left msg
    where
       msg = "Database error: " ++ (seErrorMsg sqlError)

handleXmlError :: XmlException -> IO (Either String a)
handleXmlError (MissingRequiredAttr elemName attrName) =
    do putStrLn msg
       return $ Left msg
    where
        msg = "Missing xml attribute '" ++ attrName ++ "' on element of type '" ++ elemName ++ "'"