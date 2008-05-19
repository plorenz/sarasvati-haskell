
> module WorkflowLoad where
> import qualified Data.Map as Map
> import Workflow

ArcType enumerates the kind of external allows, which are just outgoing arcs and incoming arcs.
General arcs are all defined as outgoing, but because external arcs must add outgoing arcs to
nodes not in the same workflow, they are allowed both.

> data ArcType = InArc | OutArc
>   deriving (Show)

The NodeArcs gives both incoming and outgoing nodes. However, when loading a graph, the arcs are
defined only one way (for simplicity and correctness). The LoadNode maps to what a workflow graph
definition node mostly likely looks like.

> data LoadNode a =
>     LoadNode {
>         wfNode       :: Node a,
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

> wfNodeId = nodeId.wfNode

Once we have a raw list of LoadNodes, we can complete loading it. A 'raw' LoadNode will only have it's arcRefs
populated and not have loaded externalArcs. We can fill in the arcs based on the arcRefs (internal references)
and externalArcs.
Once this is done, we can complete the load by converting the LoadNodes to NodeArcs

> completeLoad loadFunction source unlinkedNodeMap =
>     do maybeExternal <- loadExternalWorkflows loadFunction (Map.elems linkedNodeMap) (wfDepth source)
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
>                            return $ Right $ loadNodesToWfGraph resolvedLoadNodes
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
>                    Right graph -> do putStrLn $ "Loaded: " ++ (showGraph graph)
>                                      return $ Right $ Map.insert key graph wfMap
>                    Left  msg   -> return $ Left msg
>     where
>         key = targetInstance extArc
>         source = NodeSource (targetWf extArc) (targetVersion extArc) (targetInstance extArc) (depth + 1)


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
>         xmlNodes = map (importXmlNode (nextId - 1)) $ zip [nextId..] (Map.elems graph)

> importXmlNode baseIncr (nextId, nodeArcs) = LoadNode node (map (toFixedIdOutput) (nodeOutputs nodeArcs)) [] []
>     where
>         node       = (fixId.arcsNode) nodeArcs
>         fixId node = node { nodeId = nextId }
>         toFixedIdOutput (n,x) = (n,((+baseIncr).nodeId) x)

> findNodeArcs nodeMap = foldr (lookupArcs) nodeMap (Map.elems nodeMap)
>     where
>         lookupArcs node nodeMap = Map.insert (wfNodeId node) (node {arcs=(arcList node)}) nodeMap
>         arcList node            = map (makeArcRef) (arcRefs node)
>         makeArcRef (name, ref)  = (name, lookupArcRef ref)
>         lookupArcRef ref        = (wfNodeId.head) $ filter (isRefNode ref) (Map.elems nodeMap)
>         isRefNode ref loadNode  = ref == (nodeRefId.wfNode) loadNode

> resolveAllExternalArcs nodeMap = foldr (resolveNodeExternals) nodeMap (Map.elems nodeMap)

> resolveNodeExternals node nodeMap = foldr (resolveNodeExternal node) nodeMap (externalArcs node)

> resolveNodeExternal node extArc nodeMap = Map.insert (wfNodeId newNode) newNode nodeMap
>     where
>         newNode          = case (arcType extArc) of
>                                OutArc -> node { arcs = newEntry targetNode:(arcs node) }
>                                InArc  -> targetNode { arcs = newEntry node:(arcs targetNode) }
>         targetNode       = head $ filter (isMatch) (Map.elems nodeMap)
>         isMatch loadNode = (nodeRefId.wfNode)         loadNode == (targetNodeRef  extArc) &&
>                            (wfInstance.source.wfNode) loadNode == (targetInstance extArc) &&
>                            (wfDepth.source.wfNode)    loadNode == 1
>         newEntry n       = (arcName extArc, wfNodeId n)

The following function deal with converting a map of XmlNode instances to
a WfGraph. Since XmlNode instances only track outgoing nodes, we need to
infer the incoming nodes.

> loadNodesToWfGraph = graphFromArcs.loadNodesToNodeArcs

> loadNodesToNodeArcs nodeMap = map (loadNodeToNodeArcs nodeMap) (Map.elems nodeMap)

> loadNodeToNodeArcs nodeMap loadNode = NodeArcs (wfNode loadNode) inputs outputs
>     where
>         inputs            = loadNodeInputs loadNode nodeMap
>         outputs           = map (toNode) $ arcs loadNode
>         mapLookup         = (Map.!) nodeMap
>         toNode (name, id) = (name, (wfNode.mapLookup) id)

> loadNodeInputs loadNode nodeMap = foldr (scanArcs) [] $ Map.elems nodeMap
>     where
>         scanArcs node inputs        = foldr (maybeAddArc node) inputs (arcs node)
>         maybeAddArc node arc inputs = if (snd arc == targetNodeId)
>                                           then (fst arc, wfNode node):inputs
>                                           else inputs
>         targetNodeId                = wfNodeId loadNode