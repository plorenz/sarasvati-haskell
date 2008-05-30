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


module Workflow.Loaders.WorkflowLoadXml where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types
import Workflow.Engine
import qualified Data.Map as Map
import Workflow.Util.XmlUtil as XmlUtil
import Control.Monad.Error
import Workflow.Loaders.WorkflowLoad
import Workflow.Util.ListUtil

readArcs :: Element -> [(String, String)]
readArcs element = map (toArc) arcChildren
    where
        arcChildren   = XmlUtil.toElem $ ((tag "arc") `o` children) (CElem element)
        toArc e       = (readOptionalAttr e "name" "", readAttr e "to")

readExternalArcs :: Element -> [ExternalArc]
readExternalArcs element = map (readExternalArcFromElem) childElem
    where
        childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

readExternalArcFromElem :: Element -> ExternalArc
readExternalArcFromElem e = ExternalArc nodeId workflowId version instanceId arcName arcType
    where
       workflowId = readAttr e "workflow"
       version    = readAttr e "version"
       instanceId = readAttr e "instance"
       nodeId     = readAttr e "nodeId"
       arcName    = readOptionalAttr e "name" ""
       arcType    = case (readAttr e "type") of
                        "in" -> InArc
                        _    -> OutArc

-- loadWfGraphFromFile
--   Loads a WfGraph from the given file, using the given map of tag names to functions.

loadWfGraphFromFile :: String -> Map.Map String (Element -> NodeSource -> Node) -> IO (Either String WfGraph)
loadWfGraphFromFile filename funcMap =
    do xmlStr <- readFile filename
       case (xmlParse' filename xmlStr) of
           Left msg -> return $ Left msg
           Right doc -> loadWfGraphFromDoc doc (createNodeSource doc) funcMap

createNodeSource :: Document -> NodeSource
createNodeSource doc = NodeSource name "" "0" 0
    where
       root = rootElement doc
       name = readAttr root "id"

-- Given a name and a version number, this function will return the corresponding XML document.

loadXmlForWorkflow :: String -> String -> IO (Either String Document)
loadXmlForWorkflow name version =
    do xmlStr <- readFile filename
       return $ xmlParse' filename xmlStr
    where
        filename = wfDir ++ name ++ "." ++ version ++ ".wf.xml"
        wfDir = "/home/paul/workspace/functional-workflow/test-wf/"

loadWfGraph :: Map.Map String (Element -> NodeSource -> Node) -> NodeSource -> IO (Either String WfGraph)
loadWfGraph funcMap source =
    do maybeDoc <- loadXmlForWorkflow (wfName source) (wfVersion source)
       case (maybeDoc) of
           Right doc -> loadWfGraphFromDoc doc source funcMap
           Left  msg -> return $ Left msg

-- The following functions handle the generation of a WfGraph based on an XML document.
-- The loadWfGraphFromDoc function takes a map of tag names to function which take
-- elements of that type and return the appropriate XmlNode.

loadWfGraphFromDoc :: Document -> NodeSource -> Map.Map String (Element -> NodeSource -> Node) -> IO (Either String WfGraph)
loadWfGraphFromDoc doc source funcMap = completeLoad loadFunction source unlinkedNodeMap
    where
        childNodes      = getChildren (rootElement doc)
        unlinkedNodeMap = processChildNodes childNodes source funcMap Map.empty 1
        loadFunction    = loadWfGraph funcMap

processChildNodes ::
     [Element] ->
     NodeSource ->
     Map.Map String (Element -> NodeSource -> Node) ->
     Map.Map Int LoadNode ->
     Int ->
     Map.Map Int LoadNode
processChildNodes []       _      _       nodeMap _      = nodeMap
processChildNodes (e:rest) source funcMap nodeMap nextId = processChildNodes rest source funcMap newNodeMap (nextId + 1)
    where
        elemName     = case (e) of (Elem name _ _ ) -> name
        nodeFunction = funcMap Map.! elemName
        node         = fixId $ nodeFunction e source
        xmlNode      = LoadNode node [] (readArcs e) (readExternalArcs e)
        newNodeMap   = Map.insert (nodeId node) xmlNode nodeMap
        fixId  node  = node {nodeId = nextId}

-- Function for processing the start element. There should be exactly one of these
-- per workflow definition. It should contain only arc and externalArc elements. It
-- has no attributes

processStartElement :: Element -> NodeSource -> Node
processStartElement element source = Node 0 "start" "start" source False guard NoNodeExtra
    where
        guard = trim $ readText element "guard"

-- Function for processing node elements. There can be any number of these in each
-- workflow. They have no logic associated with them. They have a nodeId, which
-- should be unique in that workflow and a type, which corresponds to the NodeType
-- type in Workflow. Nodes should contain only arc and externalArc elements.

processNodeElement :: Element -> NodeSource -> Node
processNodeElement element source = Node 0 "node" nodeId source isJoinNode guard NoNodeExtra
    where
        nodeId      = readAttr element "nodeId"
        nodeTypeS   = readAttr element "type"
        guard       = readText element "guard"
        isJoinNode  = case ( nodeTypeS ) of
                          "requireSingle" -> False
                          _               -> True


defaultElemFunctionMap :: Map.Map String (Element -> NodeSource -> Node)
defaultElemFunctionMap = Map.fromList [ ("start", processStartElement),
                                        ("node",  processNodeElement) ]

elemMapWith :: [(String, (Element -> NodeSource -> Node))] -> Map.Map String (Element -> NodeSource -> Node)
elemMapWith list = addToMap list defaultElemFunctionMap
   where
       addToMap []     map = map
       addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map