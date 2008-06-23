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

module Workflow.Sarasvati.Loader where

import Control.Monad

import Data.Map as Map hiding (null, filter, map)

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import Workflow.Sarasvati.Engine
import Workflow.Sarasvati.Error
import Workflow.Sarasvati.XmlUtil

-------------------------------------------------------------------------------
--             XML Literals to  XML Data Structures                          --
-------------------------------------------------------------------------------

data XmlWorkflow =
    XmlWorkflow {
      xmlWfName  :: String,
      xmlWfNodes :: [XmlNode]
    }
  deriving Show

-- | An 'XmlNode' stores the data from a <node> element.
data XmlNode =
    XmlNode {
        xmlNodeName     :: String,
        xmlNodeType     :: String,
        xmlNodeIsJoin   :: Bool,
        xmlNodeIsStart  :: Bool,
        xmlNodeGuard    :: String,
        xmlArcs         :: [XmlArc],
        xmlExternalArcs :: [XmlExternalArc],
        xmlNodeExtra    :: NodeExtra
    }
  deriving Show

-- | An 'XmlArc' stores the data from an <arc> element.
data XmlArc =
    XmlArc {
        xmlArcName :: String,
        xmlArcTo   :: String
   }
 deriving Show


-- | Enumerates the kind of external arcs allowed, which are just outgoing arcs and incoming arcs.
--   Internal arcs are all defined as outgoing, but because external arcs must add arcs to
--   nodes not in the same workflow, they are allowed both.

data XmlExternalArcType = InArc | OutArc
  deriving Show

-- | Contains all the information we need to load an external referenced workflow and
--   import it into the currently loading workflow.

data XmlExternalArc =
    XmlExternalArc {
      xmlExtArcName     :: String,
      xmlExtArcExternal :: String,
      xmlExtArcInstance :: String,
      xmlExtArcNodeName :: String,
      xmlExtArcType     :: XmlExternalArcType
    }
 deriving Show

-- Given a filename, this function will return the corresponding XmlWorkflow.

loadXmlWorkflowFromFile :: String -> (Map.Map String (Element -> NodeExtra)) -> IO (Either String XmlWorkflow)
loadXmlWorkflowFromFile filename funcMap =
    do xmlStr <- readFile filename
       case (xmlParse' filename xmlStr) of
           Left msg  -> return $ Left msg
           Right doc -> loadXmlWorkflow doc funcMap

loadWfGraphFromFile :: (Loader l) => l -> String -> (Map.Map String (Element -> NodeExtra)) -> IO (Either String WfGraph)
loadWfGraphFromFile loader filename funcMap =
    do xmlStr <- readFile filename
       case (xmlParse' filename xmlStr) of
           Left msg  -> return $ Left msg
           Right doc -> do result <- loadXmlWorkflow doc funcMap
                           case result of
                               Left msg    -> return $ Left msg
                               Right xmlWf -> do wfGraph <- processXmlWorkflow loader xmlWf
                                                 return (Right wfGraph)

loadXmlWorkflow :: Document -> (Map.Map String (Element -> NodeExtra)) -> IO (Either String XmlWorkflow)
loadXmlWorkflow doc funcMap =
    do catchWf (return xmlWorkflow)
    where
        xmlWorkflow  = Right $ XmlWorkflow name nodes
        root         = rootElement doc
        name         = readRequiredAttr root "name"
        nodes        = loadXmlNodes (getChildren root) funcMap

loadXmlNodes :: [Element] -> (Map.Map String (Element -> NodeExtra)) -> [XmlNode]
loadXmlNodes [] _             = []
loadXmlNodes (e:rest) funcMap = xmlNode : loadXmlNodes rest funcMap
    where
        xmlNode      = XmlNode name nodeType isJoin isStart guard arcs externalArcs nodeExtra
        name         = readRequiredAttr e "name"
        nodeTypeOpt  = readOptionalAttr e "type" "node"
        nodeType     = case nodeTypeOpt of
                           [] -> "node"
                           _  -> nodeTypeOpt
        isJoin       = case (readOptionalAttr e "isJoin" "false" ) of
                           "true" -> True
                           _      -> False
        isStart      = case (readOptionalAttr e "isStart" "false" ) of
                           "true" -> True
                           _      -> False
        guard        = readText e "guard"
        arcs         = loadXmlArcs         $ getChildrenNamed e "arc"
        externalArcs = loadXmlExternalArcs $ getChildrenNamed e "externalArc"
        nodeExtra    = case (Map.member nodeType funcMap) of
                           False -> NoNodeExtra
                           True  -> (funcMap Map.! nodeType) e

loadXmlArcs :: [Element] -> [XmlArc]
loadXmlArcs []       = []
loadXmlArcs (e:rest) =  xmlArc : loadXmlArcs rest
    where
        xmlArc = XmlArc name to
        name   = readOptionalAttr e "name" ""
        to     = readRequiredAttr e "to"


loadXmlExternalArcs :: [Element] -> [XmlExternalArc]
loadXmlExternalArcs []       = []
loadXmlExternalArcs (e:rest) =  xmlExternalArc : loadXmlExternalArcs rest
    where
        xmlExternalArc = XmlExternalArc name external inst nodeName arcType
        name           = readOptionalAttr e "name" ""
        external       = readRequiredAttr e "external"
        inst           = readRequiredAttr e "instance"
        nodeName       = readRequiredAttr e "nodeName"
        arcType        = case (readRequiredAttr e "type" ) of
                             "in"  -> InArc
                             "out" -> OutArc
                             _     -> wfError $ "Invalid value for external arc 'type' attribute specified. Must be 'in' or 'out'."

-------------------------------------------------------------------------------
--             XML Data structures to Engine Data structures                 --
-------------------------------------------------------------------------------

data LoadNode =
    LoadNode {
        loadedNode   :: Node,
        xmlNode      :: XmlNode
    }

class Loader loader where
    createWorkflow :: loader -> XmlWorkflow -> IO Int
    createNode     :: loader -> Int -> XmlNode -> IO Node
    createArc      :: loader -> Int -> String -> Node -> Node -> IO Arc
    importInstance :: loader -> Int -> String -> String -> IO ([Node],[Arc])

class Resolver resolver where
    resolveGraphNameToXmlWorkflow  :: resolver -> String -> IO XmlWorkflow

processXmlWorkflow :: (Loader l) => l -> XmlWorkflow -> IO WfGraph
processXmlWorkflow loader xmlWf =
    do graphId     <- createWorkflow loader xmlWf
       nodes       <- mapM (xmlNodeToLoadNode loader graphId) xmlNodes
       arcs        <- createInternalArcs loader graphId nodes
       instanceMap <- foldM (importExternal loader graphId) Map.empty externals
       extArcs     <- createExternalArcs loader graphId nodes instanceMap
       return $ graphFromArcs graphId
                              (xmlWfName xmlWf)
                              (extractNodes nodes instanceMap)
                              (extractArcs  arcs  extArcs instanceMap)
    where
        xmlNodes  = xmlWfNodes xmlWf
        externals = concatMap (xmlExternalArcs) xmlNodes

extractNodes :: [LoadNode] -> Map.Map String ([Node],[Arc]) -> [Node]
extractNodes loadNodes nodeMap = nodes ++ fixedExtNodes
    where
        nodes         = map (loadedNode) loadNodes
        instanceNodes = concatMap (fst) (Map.elems nodeMap)
        fixedExtNodes = map (\node -> node { nodeIsExternal = True } ) instanceNodes

extractArcs :: [Arc] -> [Arc] -> Map.Map String ([Node],[Arc]) -> [Arc]
extractArcs arcs extArcs instanceMap = arcs ++ extArcs ++ instanceArcs
    where
        instanceArcs = concatMap (snd) (Map.elems instanceMap)

instanceKey :: XmlExternalArc -> String
instanceKey extArc = (xmlExtArcExternal extArc) ++ ":" ++ (xmlExtArcInstance extArc)

importExternal :: (Loader l) => l -> Int -> Map.Map String ([Node],[Arc]) -> XmlExternalArc -> IO (Map.Map String ([Node],[Arc]))
importExternal loader graphId instanceMap extArc =
    if (Map.member key instanceMap)
        then return instanceMap
        else do pair <- importInstance loader graphId graphName instanceName
                return $ Map.insert key pair instanceMap
    where
        key          = instanceKey extArc
        graphName    = xmlExtArcExternal extArc
        instanceName = xmlExtArcInstance extArc

xmlNodeToLoadNode :: (Loader l) => l -> Int -> XmlNode -> IO LoadNode
xmlNodeToLoadNode loader graphId xmlNode =
    do node <- createNode loader graphId xmlNode
       return $ LoadNode node xmlNode

createInternalArcs :: (Loader l) => l -> Int -> [LoadNode] -> IO [Arc]
createInternalArcs loader graphId loadNodes =
    do arcList <- mapM (resolveArcs') loadNodes
       return (concat arcList)
    where
        resolveArcs' loadNode = mapM (createInternalArc loader loadNode graphId loadNodes) ((xmlArcs.xmlNode) loadNode)

createInternalArc :: (Loader l) => l -> LoadNode -> Int -> [LoadNode] -> XmlArc -> IO Arc
createInternalArc loader node graphId loadNodes xmlArc
    | noTarget       = wfError $ "No node with name " ++ targetName ++
                                 " found while looking for arc endpoint"
    | tooManyTargets = wfError $ "Too many nodes with name " ++ targetName ++
                                 " found while looking for arc endpoint"
    | otherwise      = createArc loader graphId arcName startNode endNode
    where
        arcName        = xmlArcName xmlArc
        targetName     = xmlArcTo   xmlArc
        targetNodes    = filter (\n->(xmlNodeName.xmlNode) n == targetName) loadNodes
        noTarget       = null targetNodes
        tooManyTargets = length targetNodes > 1
        startNode      = loadedNode node
        endNode        = loadedNode (head targetNodes)

createExternalArcs :: (Loader l) => l -> Int -> [LoadNode] -> Map.Map String ([Node],[Arc]) -> IO [Arc]
createExternalArcs loader graphId nodes instanceMap = mapM (createExternalArc loader graphId instanceMap) nodeArcPairs
    where
        nodeArcPairs = concatMap (\loadNode-> map(\arc -> (loadedNode loadNode,arc)) ((xmlExternalArcs.xmlNode) loadNode)) nodes

createExternalArc :: (Loader l) => l -> Int -> Map.Map String ([Node],[Arc]) -> (Node, XmlExternalArc) -> IO Arc
createExternalArc loader graphId instanceMap (node,extArc)
    | not (Map.member instKey instanceMap) = error $ "Instance not found: " ++ instKey
    | noTarget       = wfError $ "No node with name " ++ targetName ++
                                 " found in external " ++ (xmlExtArcExternal extArc) ++
                                 " while looking for external arc endpoint"
    | tooManyTargets = wfError $ "Too many nodes with name " ++ targetName ++
                                 " found in external " ++ (xmlExtArcExternal extArc) ++
                                 " while looking for external arc endpoint"
    | otherwise      = case (xmlExtArcType extArc) of
                            InArc  -> createArc loader graphId extArcName targetNode node
                            OutArc -> createArc loader graphId extArcName node       targetNode
    where
        instKey        = instanceKey extArc
        nodes          = fst $ instanceMap Map.! instKey
        extArcName     = xmlExtArcName extArc
        targetName     = xmlExtArcNodeName extArc
        targetNodes    = filter (\n -> (nodeName n) == targetName && ((not.nodeIsExternal) n)) nodes
        noTarget       = null targetNodes
        tooManyTargets = length targetNodes > 1
        targetNode     = head targetNodes

xmlNodeToNode :: Int -> XmlNode -> Node
xmlNodeToNode nodeId xmlNode =
    Node nodeId
         (xmlNodeType xmlNode)
         (xmlNodeName xmlNode)
         (xmlNodeIsJoin xmlNode)
         (xmlNodeIsStart xmlNode)
         False
         (xmlNodeGuard xmlNode)
         (xmlNodeExtra xmlNode)


data SimpleFileResolver =
    SimpleFileResolver {
        basePath :: String,
        funcMap  :: Map.Map String (Element -> NodeExtra)
    }

instance Resolver SimpleFileResolver where
    resolveGraphNameToXmlWorkflow resolver name =
        do result <- loadXmlWorkflowFromFile path (funcMap resolver)
           case result of
               Left msg    -> wfError $ "Failed to load '" ++ name ++ "' from file '" ++ path ++
                                        "' because: " ++ msg
               Right xmlWf -> return $ xmlWf
        where
            path = (basePath resolver) ++ name ++ ".wf.xml"

{-
testLoad :: String -> IO ()
testLoad f =
    do res <- loadXmlWorkflowFromFile f Map.empty
       case res of
           Left msg    -> putStrLn msg
           Right wfxml -> putStrLn (show wfxml)
-}