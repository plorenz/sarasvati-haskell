
> module WorkflowLoadXml where
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Workflow
> import qualified Data.Map as Map
> import Control.Monad
> import XmlUtil
> import Control.Monad.Error
> import WorkflowLoad

> readArcs element = map (toArc) arcChildren
>     where
>         attrVal (v,_) = v
>         arcChildren   = XmlUtil.toElem $ ((tag "arc") `o` children) (CElem element)
>         toArc e       = (readOptionalAttr e "name" "", readAttr e "to")
>

> readExternalArcs element = map (readExternalArcFromElem) childElem
>     where
>         childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem e = ExternalArc nodeId workflowId version instanceId arcName arcType
>     where
>        workflowId = readAttr e "workflow"
>        version    = readAttr e "version"
>        instanceId = readAttr e "instance"
>        nodeId     = readAttr e "nodeId"
>        arcName    = readOptionalAttr e "name" ""
>        arcType    = case (readAttr e "type") of
>                         "in"      -> InArc
>                         otherwise -> OutArc

loadWfGraphFromFile
  Loads a WfGraph from the given file, using the given map of tag names to functions.

> loadWfGraphFromFile filename funcMap =
>     do xmlStr <- readFile filename
>        case (xmlParse' filename xmlStr) of
>            Left msg -> return $ Left msg
>            Right doc -> loadWfGraphFromDoc doc (createNodeSource doc) funcMap

> createNodeSource doc = NodeSource name "" "0" 0
>     where
>        root = rootElement doc
>        name = readAttr root "id"

Given a name and a version number, this function will return the corresponding XML document.

> loadXmlForWorkflow name version =
>     do xmlStr <- readFile filename
>        return $ xmlParse' filename xmlStr
>     where
>         filename = wfDir ++ name ++ "." ++ version ++ ".wf.xml"
>         wfDir = "/home/paul/workspace/functional-workflow/test-wf/"

> loadWfGraph funcMap source =
>     do maybeDoc <- loadXmlForWorkflow (wfName source) (wfVersion source)
>        case (maybeDoc) of
>            Right doc -> loadWfGraphFromDoc doc source funcMap
>            Left  msg -> return $ Left msg

The following functions handle the generation of a WfGraph based on an XML document.
The loadWfGraphFromDoc function takes a map of tag names to function which take
elements of that type and return the appropriate XmlNode.

> loadWfGraphFromDoc doc source funcMap = completeLoad (loadWfGraph funcMap) source unlinkedNodeMap
>     where
>         childNodes      = getChildren (rootElement doc)
>         unlinkedNodeMap = processChildNodes childNodes source funcMap Map.empty 1
>

> processChildNodes []       _      _       nodeMap nextId = nodeMap
> processChildNodes (e:rest) source funcMap nodeMap nextId = processChildNodes rest source funcMap newNodeMap (nextId + 1)
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = funcMap Map.! elemName
>         node         = fixId $ nodeFunction e source
>         xmlNode      = LoadNode node [] (readArcs e) (readExternalArcs e)
>         newNodeMap   = Map.insert (nodeId node) xmlNode nodeMap
>         fixId  node  = case (nodeId node) of
>                            (-1) -> node
>                            otherwise -> node {nodeId = nextId}

Function for processing the start element. There should be exactly one of these
per workflow definition. It should contain only arc and externalArc elements. It
has no attributes

> processStartElement element source = Node 0 "start" "start" source False NoNodeExtra

Function for processing node elements. There can be any number of these in each
workflow. They have no logic associated with them. They have a nodeId, which
should be unique in that workflow and a type, which corresponds to the NodeType
type in Workflow. Nodes should contain only arc and externalArc elements.

> processNodeElement element source = Node 0 "node" nodeId source isJoinNode NoNodeExtra
>     where
>         nodeId      = readAttr element "nodeId"
>         nodeTypeS   = readAttr element "type"
>         isJoinNode  = case ( nodeTypeS ) of
>                           "requireSingle" -> False
>                           otherwise       -> True


> defaultElemFunctionMap = Map.fromList [ ("start", processStartElement),
>                                         ("node",  processNodeElement) ]

> elemMapWith list = addToMap list defaultElemFunctionMap
>    where
>        addToMap []     map = map
>        addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map