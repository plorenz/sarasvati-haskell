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

module Workflow.Loader where

import Control.Exception

import Data.Dynamic
import qualified Data.Map as Map

import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

import Workflow.Engine
import Workflow.Util.XmlUtil

-------------------------------------------------------------------------------
--             XML Literals to  XML Data Structures                          --
-------------------------------------------------------------------------------

data XmlWorkflow =
    XmlWorkflow {
      xmlWfName  :: String,
      xmlWfNodes :: [XmlNode]
    }
  deriving Show

data XmlNodeExtra = NoXmlNodeExtra | XmlNodeExtra Dynamic
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
        xmlNodeExtra    :: XmlNodeExtra
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

loadXmlWorkflowFromFile :: String -> (Map.Map String (Element -> XmlNodeExtra)) -> IO (Either String XmlWorkflow)
loadXmlWorkflowFromFile filename funcMap =
    do xmlStr <- readFile filename
       case (xmlParse' filename xmlStr) of
           Left msg  -> return $ Left msg
           Right doc -> loadXmlWorkflow doc funcMap

loadXmlWorkflow :: Document -> (Map.Map String (Element -> XmlNodeExtra)) -> IO (Either String XmlWorkflow)
loadXmlWorkflow doc funcMap =
    do handleErrors (return xmlWorkflow)
    where
        xmlWorkflow  = Right $ XmlWorkflow name nodes
        root         = rootElement doc
        name         = readRequiredAttr root "name"
        nodes        = loadXmlNodes (getChildren root) funcMap
        handleErrors = (handleWfLoad wfErrorHandler).(handleXml xmlErrorHandler)

        wfErrorHandler (WfLoadError msg) = return $ Left msg
        xmlErrorHandler (XmlError msg)   = return $ Left msg

loadXmlNodes :: [Element] -> (Map.Map String (Element -> XmlNodeExtra)) -> [XmlNode]
loadXmlNodes [] _             = []
loadXmlNodes (e:rest) funcMap = xmlNode : loadXmlNodes rest funcMap
    where
        xmlNode      = XmlNode name nodeType isJoin isStart guard arcs externalArcs nodeExtra
        name         = readRequiredAttr e "name"
        nodeType     = readOptionalAttr e "type" "node"
        isJoin       = case (readOptionalAttr e "isJoin" "false" ) of
                           "true" -> True
                           _      -> False
        isStart      = case (readOptionalAttr e "isStart" "false" ) of
                           "true" -> True
                           _      -> False
        guard        = readText e "guard"
        arcs         = loadXmlArcs         $ toElem $ ((tag "arc") `o` children) (CElem e)
        externalArcs = loadXmlExternalArcs $ toElem $ ((tag "externalArc") `o` children) (CElem e)
        nodeExtra    = case (Map.member nodeType funcMap) of
                           False -> NoXmlNodeExtra
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
        name           = readRequiredAttr e "name"
        external       = readRequiredAttr e "external"
        inst           = readRequiredAttr e "instance"
        nodeName       = readRequiredAttr e "nodeName"
        arcType        = case (readRequiredAttr e "type" ) of
                             "in"  -> InArc
                             "out" -> OutArc
                             _     -> wfLoadError $ "Invalid value for external arc 'type' attribute specified. Must be 'in' or 'out'."

-------------------------------------------------------------------------------
--             XML Data structures to Engine Data structures                 --
-------------------------------------------------------------------------------

data LoadNode =
    LoadNode {
        loadedNode   :: Node,
        xmlNode      :: XmlNode,
        loadNodeArcs :: [LoadArc]
    }

data LoadArc =
    LoadArc {
        arcName   :: String,
        arcNodeId :: Int
    }

class Loader loader where
    loadWorkflow :: loader -> XmlWorkflow -> IO Int
    loadNode     :: loader -> Int -> XmlNode -> IO Node

processXmlWorkflow :: (Loader l) => l -> XmlWorkflow -> IO ()
processXmlWorkflow loader xmlWf =
    do graphId <- loadWorkflow loader xmlWf
       nodes   <- mapM (xmlNodeToLoadNode loader graphId) (xmlWfNodes xmlWf)
       let nodes = resolveArcs nodes
       return ()

xmlNodeToLoadNode :: (Loader l) => l -> Int -> XmlNode -> IO LoadNode
xmlNodeToLoadNode loader graphId xmlNode =
    do node <- loadNode loader graphId xmlNode
       return $ LoadNode node xmlNode []

resolveArcs :: [LoadNode] -> [LoadNode]
resolveArcs loadNodes = map (resolveArcs') loadNodes
    where
        resolveArcs' loadNode = loadNode {loadNodeArcs = map (resolveArc loadNodes) ((xmlArcs.xmlNode) loadNode) }

resolveArc :: [LoadNode] -> XmlArc -> LoadArc
resolveArc loadNodes xmlArc
    | noTarget      = wfLoadError $ "No node with name " ++ targetName ++
                                  " found while looking for arc endpoint"
    | toManyTargets = wfLoadError $ "Too many nodes with name " ++ targetName ++
                                  " found while looking for arc endpoint"
    | otherwise     = LoadArc arcName $ (nodeId.loadedNode.head) targetNodes
    where
        arcName       = xmlArcName xmlArc
        targetName    = xmlArcTo   xmlArc
        targetNodes   = filter (\n->(xmlNodeName.xmlNode) n == targetName) loadNodes
        noTarget      = null targetNodes
        toManyTargets = length targetNodes > 1

xmlNodeToNode :: Int -> XmlNode -> NodeExtra -> Node
xmlNodeToNode nodeId xmlNode nodeExtra =
    Node nodeId
         (xmlNodeType xmlNode)
         (xmlNodeName xmlNode)
         (xmlNodeIsJoin xmlNode)
         (xmlNodeIsStart xmlNode)
         (xmlNodeGuard xmlNode)
         nodeExtra

data WfLoadError = WfLoadError String
  deriving (Show,Typeable)

wfLoadError :: String -> a
wfLoadError msg = throwDyn $ WfLoadError msg

handleWfLoad :: (WfLoadError -> IO a) -> IO a -> IO a
handleWfLoad f a = catchDyn a f

{-
testLoad f =
    do res <- loadXmlWorkflowFromFile f Map.empty
       case res of
           Left msg    ->  putStrLn msg
           Right wfxml -> putStrLn (show wfxml)
-}