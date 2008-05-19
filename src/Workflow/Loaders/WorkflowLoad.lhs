
> module Workflow.Loaders.WorkflowLoad where
> import qualified Data.Map as Map
> import qualified Workflow.Workflow as Workflow

ArcType enumerates the kind of external allows, which are just outgoing arcs and incoming arcs.
General arcs are all defined as outgoing, but because external arcs must add outgoing arcs to
nodes not in the same workflow, they are allowed both.

> data ArcType = InArc | OutArc
>   deriving (Show)

The NodeArcs gives both incoming and outgoing nodes. However, when loading a graph, the arcs are
defined only one way (for simplicity and correctness). The LoadNode maps to what a workflow graph
definition node mostly likely looks like.

> data LoadNode =
>     LoadNode {
>         wfNode       :: Workflow.Node,
>         arcs         :: [(String,Int)],
>         arcRefs      :: [(String,String)],
>         externalArcs :: [ExternalArc]
>     }
>  deriving (Show)

The ExternalArc contains all the information we need to load an external referenced workflow and
import it into the currently loading workflow.

> data ExternalArc =
>     ExternalArc {
>       targetNodeRef  :: String,
>       targetWf       :: String,
>       targetVersion  :: String,
>       targetInstance :: String,
>       arcName        :: String,
>       arcType        :: ArcType
>     }
>  deriving (Show)

Shortcut function to get the nodeId of the Node in a LoadNode

> wfNodeId = Workflow.nodeId.wfNode

Once we have a raw list of LoadNodes, we can complete loading it. A 'raw' LoadNode will only have it's arcRefs
populated and not have loaded externalArcs. We can fill in the arcs based on the arcRefs (internal references)
and externalArcs.
Once this is done, we can complete the load by converting the LoadNodes to NodeArcs

> completeLoad loadFunction source unlinkedNodeMap =
>     do maybeExternal <- loadExternalWorkflows loadFunction (Map.elems linkedNodeMap) (Workflow.wfDepth source)
>        case (maybeExternal) of
>            Left msg  -> return $ Left msg
>            Right ext -> do putStrLn "Unlinked before externals: "
>                            putStrLn (show unlinkedNodeMap)
>                            putStrLn "Linked before externals: "
>                            putStrLn (show linkedNodeMap)
>                            let allLoadNodes = importExternals linkedNodeMap ext
>                            putStrLn "\nAfter import of externals"
>                            putStrLn (show allLoadNodes)
>                            let resolvedLoadNodes = resolveAllExternalArcs allLoadNodes
>                            putStrLn "\nAfter resolving arcs"
>                            putStrLn (show resolvedLoadNodes)
>                            return $ Right $ loadNodesToWfGraph (Workflow.wfName source) resolvedLoadNodes
>    where
>       linkedNodeMap = findNodeArcs unlinkedNodeMap

loadExternalWorkflows
   Returns a map of the external workflows referenced by a workflow, keyed by instance id.

   loadFunction - Function which takes a NodeSource and Maybe returns the loaded workflow
   loadNodes    - Map of LoadNodes in the workflow we are loading, keyed by node id
   depth        - How deep we are in the loading process. The top level workflow has a depth
                  of 0. Referenced, external workflows have a depth of 1. Workflows referenced
                  from the referenced workflows have a depth of 2 and so on.

> loadExternalWorkflows loadFunction loadNodes depth = foldr (checkNodes) startMap loadNodes
>     where
>         checkNodes loadNode wfMap    = foldr (checkArcs) wfMap (externalArcs loadNode)
>         checkArcs extArc maybeMapIO = do maybeMap <- maybeMapIO
>                                          case (maybeMap) of
>                                              Right wfMap -> loadExternal loadFunction wfMap extArc depth
>                                              Left  msg   -> return $ Left msg
>         startMap = return (Right Map.empty)

> loadExternal loadFunction wfMap extArc depth =
>     if (Map.member key wfMap)
>        then do return $ Right wfMap
>        else do putStrLn $ "Loading " ++ (targetWf extArc) ++ " version: " ++ (targetVersion extArc)
>                maybeGraph <- loadFunction source
>                case (maybeGraph) of
>                    Right graph -> do putStrLn $ "Loaded: " ++ (Workflow.showGraph graph)
>                                      return $ Right $ Map.insert key graph wfMap
>                    Left  msg   -> return $ Left msg
>     where
>         key = targetInstance extArc
>         source = Workflow.NodeSource (targetWf extArc) (targetVersion extArc) (targetInstance extArc) (depth + 1)


To import an external workflow into loading workflow, we must take the following steps:
  1. Convert the nodes back to XmlNodes

     Because the node ids will overlap with those in the top level graph, we must assign new ids.
     We can just start the numbering where we left off. Because the ids will all increase by the
     same amount we can fix the incoming/outgoing arcs easily.

  2. Convert external arcs to regular arcs

> importExternals nodeMap externals = foldr (importExternal) nodeMap (Map.elems externals)

> importExternal graph nodeMap = foldr (\node nodeMap-> Map.insert (wfNodeId node) node nodeMap) nodeMap xmlNodes
>     where
>         nextId   = (Map.size nodeMap) + 1
>         xmlNodes = map (importXmlNode (nextId - 1) outputMap) $ zip [nextId..] nodeList
>         outputMap = Workflow.graphOutputArcs graph
>         nodeList  = Map.elems (Workflow.graphNodes graph)

> importXmlNode baseIncr outputMap (nextId, extNode) = LoadNode node (map (toFixedIdOutput) outputList) [] []
>     where
>         node       =  extNode { Workflow.nodeId = nextId }
>         toFixedIdOutput arc = (Workflow.arcName arc, baseIncr + (Workflow.outNodeId arc))
>         outputList = outputMap Map.! (Workflow.nodeId extNode)

> findNodeArcs nodeMap = foldr (lookupArcs) nodeMap (Map.elems nodeMap)
>     where
>         lookupArcs node nodeMap = Map.insert (wfNodeId node) (node {arcs=(arcList node)}) nodeMap
>         arcList node            = map (makeArcRef) (arcRefs node)
>         makeArcRef (name, ref)  = (name, lookupArcRef ref)
>         lookupArcRef ref        = (wfNodeId.head) $ filter (isRefNode ref) (Map.elems nodeMap)
>         isRefNode ref loadNode  = ref == (Workflow.nodeName.wfNode) loadNode

> resolveAllExternalArcs nodeMap = foldr (resolveNodeExternals) nodeMap (Map.elems nodeMap)

> resolveNodeExternals node nodeMap = foldr (resolveNodeExternal node) nodeMap (externalArcs node)

> resolveNodeExternal :: LoadNode -> ExternalArc -> Map.Map Int LoadNode -> Map.Map Int LoadNode
> resolveNodeExternal node extArc nodeMap = Map.insert (wfNodeId newNode) newNode nodeMap
>     where
>         newNode          = case (arcType extArc) of
>                                OutArc -> node { arcs = newEntry targetNode:(arcs node) }
>                                InArc  -> targetNode { arcs = newEntry node:(arcs targetNode) }
>         targetNode       = head $ filter (isMatch) (Map.elems nodeMap)
>         isMatch loadNode = (Workflow.nodeName.wfNode)                       loadNode == (targetNodeRef  extArc) &&
>                            (Workflow.wfName.Workflow.nodeSource.wfNode)     loadNode == (targetWf extArc ) &&
>                            (Workflow.wfInstance.Workflow.nodeSource.wfNode) loadNode == (targetInstance extArc) &&
>                            (Workflow.wfDepth.Workflow.nodeSource.wfNode)    loadNode == depth
>         newEntry n       = (arcName extArc, wfNodeId n)
>         depth            = (Workflow.wfDepth.Workflow.nodeSource.wfNode) node + 1

The following function deal with converting a map of XmlNode instances to
a WfGraph. Since XmlNode instances only track outgoing nodes, we need to
infer the incoming nodes.

> loadNodesToWfGraph name nodeMap = Workflow.graphFromArcs 0 name (map (wfNode) nodeList) (loadNodesToArcs 1 nodeList)
>     where
>         nodeList = Map.elems nodeMap


> loadNodesToArcs _       []              = []
> loadNodesToArcs startId (loadNode:rest) = arcList ++ (loadNodesToArcs (startId + length arcList) rest)
>     where
>         arcList = makeArcList startId ((Workflow.nodeId.wfNode) loadNode) (arcs loadNode)


> makeArcList _       _        []                         = []
> makeArcList startId inNodeId ((name, outNodeId) : rest) =
>     (Workflow.Arc startId name inNodeId outNodeId) : makeArcList (startId + 1) inNodeId rest

> loadNodeInputs loadNode nodeMap = foldr (scanArcs) [] $ Map.elems nodeMap
>     where
>         scanArcs node inputs        = foldr (maybeAddArc node) inputs (arcs node)
>         maybeAddArc node arc inputs = if (snd arc == targetNodeId)
>                                           then (fst arc, wfNode node):inputs
>                                           else inputs
>         targetNodeId                = wfNodeId loadNode