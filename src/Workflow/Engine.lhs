Author: Paul Lorenz

> module Workflow.Engine where
> import qualified Data.Map as Map
> import qualified Workflow.Util.ListUtil as ListUtil
> import Data.Dynamic

GuardResponse
  Nodes have guard functions which determine if the accept function when a token
  arrives and the node is ready to be activated. Guard functions must return a
  GuardResponse

  AcceptToken  - The token is passed on to the accept function
  DiscardToken - The token is discarded and the accept function is not called
  SkipNode     - The accept function is not called. The token is not discarded,
                 the completeExecution function is called instead.

> data GuardResponse = AcceptToken | DiscardToken | SkipNode
>   deriving (Show)

> data NodeSource =
>     NodeSource {
>         wfName     :: String,
>         wfVersion  :: String,
>         wfInstance :: String,
>         wfDepth    :: Int
>     }
>  deriving (Show, Eq)

NodeExtra is a place to store any extra data that a given node may
require. The only requirement is that the 'extra data' be a Typeable
so it can encapsulated in a Dynamic

> data NodeExtra = NoNodeExtra | NodeExtra Dynamic

> makeNodeExtra :: (Typeable a) => a -> NodeExtra
> makeNodeExtra extra = NodeExtra $ toDyn extra

> instance Show (NodeExtra) where
>     show NoNodeExtra = "NoNodeExtra"
>     show _           = "NodeExtra: Dynamic"

Node
  Represents a node in a workflow graph.

  Members:
    nodeId - An integer id, which should be unique. Used for testing equality
    accept - function which handles incoming tokens.

  Connections between Nodes are represented by Arcs and WFGraph

> data Node =
>     Node {
>         nodeId       :: Int,
>         nodeType     :: String,
>         nodeName     :: String,
>         nodeSource   :: NodeSource,
>         nodeIsJoin   :: Bool,
>         nodeExtra    :: NodeExtra
>     }

> instance Show (Node) where
>     show a = "|Node id: " ++ (show.nodeId) a ++ " name: " ++ nodeName a ++
>              " depth: " ++ (show.nodeSource) a ++ "|"

NodeType
  Encapsulates node functionality

> data NodeType a =
>     NodeType {
>         guardFunction  :: (NodeToken -> WfRun a -> GuardResponse),
>         acceptFunction :: (WfEngine engine) => (engine -> NodeToken -> WfRun a -> IO (WfRun a))
>     }

Arc
  An Arc represents an directed edge in a workflow graph.
  It has an id, a label and two node id endpoints.

> data Arc =
>     Arc {
>         arcId        :: Int,
>         arcName      :: String,
>         startNodeId  :: Int,
>         endNodeId    :: Int
>     }
>  deriving (Show)

Tokens are split into NodeTokens and ArcTokens. NodeTokens are sitting at
nodes in the workflow graph while ArcTokens are 'in-transit' and are on
Arcs.

The Token class allows NodeTokens and ArcTokens to share an id lookup function

> class Token a where
>    tokenId   :: a -> Int

NodeToken represents tokens which are at node

> data NodeToken = NodeToken Int Int
>     deriving (Show)

> instance Token (NodeToken) where
>     tokenId (NodeToken tokId _ ) = tokId

> instance Eq (NodeToken) where
>     tok1 == tok2 = (tokenId tok1) == (tokenId tok2)

ArcToken represents tokens which are between nodes (on an arc)

> data ArcToken = ArcToken Int Arc
>     deriving (Show)

> instance Token (ArcToken) where
>     tokenId (ArcToken tokId _) = tokId

> instance Eq (ArcToken) where
>     tok1 == tok2 = (tokenId tok1) == (tokenId tok2)

WFGraph
  Has the set of nodes as well as maps of node input arcs and node output arcs
  keyed by node id.

> data WfGraph =
>     WfGraph {
>        graphId         :: Int,
>        graphName       :: String,
>        graphNodes      :: Map.Map Int Node,
>        graphInputArcs  :: Map.Map Int [Arc],
>        graphOutputArcs :: Map.Map Int [Arc]
>     }

A WfRun tracks the current state of the workflow. It has the workflow graph as well
as the tokens representing the current state. A slot for user data is also defined.

> data WfRun a =
>     WfRun {
>         runId      :: Int,
>         nodeTypes  :: Map.Map String (NodeType a),
>         wfGraph    :: WfGraph,
>         nodeTokens :: [NodeToken],
>         arcTokens  :: [ArcToken],
>         userData   :: a
>     }

> class WfEngine a where
>     createWfRun         :: a -> WfGraph   -> Map.Map String (NodeType b) -> b -> IO (WfRun b)
>     createNodeToken     :: a -> WfRun b   -> Node -> [ArcToken] -> IO NodeToken
>     createArcToken      :: a -> WfRun b   -> Arc  -> NodeToken  -> IO ArcToken
>     completeNodeToken   :: a -> NodeToken -> IO ()
>     completeArcToken    :: a -> ArcToken  -> IO ()
>     transactionBoundary :: a -> IO ()

showGraph
  Print prints a graph

> showGraph :: WfGraph -> String
> showGraph graph = graphName graph ++ ":\n" ++
>                   concatMap (\a->show a ++ "\n") (Map.elems (graphNodes graph)) ++ "\n" ++
>                   concatMap (\a->show a ++ "\n") (Map.elems (graphInputArcs graph)) ++ "\n" ++
>                   concatMap (\a->show a ++ "\n") (Map.elems (graphOutputArcs graph))

graphFromNodesAndArcs
  Generates a WFGraph from a list of Nodes and Arcs

> graphFromArcs :: Int -> String -> [Node] -> [Arc] -> WfGraph
> graphFromArcs graphId name nodes arcs = WfGraph graphId name nodeMap inputsMap outputsMap
>     where
>         nodeMap  = Map.fromList $ zip (map nodeId nodes) nodes
>
>         inputsMap             = Map.fromList $ zip (map nodeId nodes) (map inputArcsForNode nodes)
>         inputArcsForNode node = filter (\arc -> endNodeId arc == nodeId node) arcs
>
>         outputsMap = Map.fromList $ zip (map nodeId nodes) (map outputArcsForNode nodes)
>         outputArcsForNode node = filter (\arc -> startNodeId arc == nodeId node) arcs

getTokenForId
  Given a token id and a workflow instance gives back the actual token
  corresponding to that id

> getNodeTokenForId :: Int -> WfRun a -> NodeToken
> getNodeTokenForId tokId wf =
>   head $ filter (\t -> (tokenId t) == tokId) (nodeTokens wf)

Convenience lookup methods for the data pointed to by tokens

> nodeForToken :: NodeToken -> WfGraph -> Node
> nodeForToken (NodeToken _ nodeId) graph = (graphNodes graph) Map.! nodeId

> arcForToken :: ArcToken -> Arc
> arcForToken  (ArcToken _ arc)           = arc

startWorkflow
  Given a workflow definition (WfGraph) and initial userData, gives
  back a new in progress workflow instance for that definition.

> startWorkflow :: (WfEngine e) => e -> Map.Map String (NodeType a) -> WfGraph -> a -> IO ( Either String (WfRun a))
> startWorkflow engine nodeTypes graph userData
>     | null startNodes       = return $ Left "Error: Workflow has no start node"
>     | length startNodes > 1 = return $ Left "Error: Workflow has more than one start node"
>     | otherwise             = do wfRun <- createWfRun engine graph nodeTypes userData
>                                  startToken <- createNodeToken engine wfRun startNode []
>                                  wfRun <- acceptWithGuard engine startToken (wfRun { nodeTokens = [startToken] })
>                                  return $ Right wfRun
>   where
>     startNodes = filter (isStartNode) $ Map.elems (graphNodes graph)
>     startNode  = head startNodes
>     isStartNode node = (nodeName node == "start") && ((wfDepth.nodeSource) node == 0)

> isWfComplete :: WfRun a -> Bool
> isWfComplete (WfRun _ _ _ [] [] _) = True
> isWfComplete _                     = False

removeInputTokens
  Given a list of input arcs, a target node id and a list of arc tokens,
  for each node removes the first token which has the input node as
  its previous node and the target node as its current node

> removeInputTokens :: [Arc] -> t -> [ArcToken] -> [ArcToken]
> removeInputTokens []         _          tokenList = tokenList
> removeInputTokens (arc:arcs) targetNodeId tokenList =
>     removeInputTokens arcs targetNodeId $ ListUtil.removeFirst (isInputToken) tokenList
>   where
>     isInputToken tok = (arcId.arcForToken) tok == arcId arc

> removeNodeToken :: NodeToken -> WfRun a -> WfRun a
> removeNodeToken token wf = wf { nodeTokens = ListUtil.removeFirst (\t->t == token) (nodeTokens wf) }

defaultGuard
  Guard function which always accepts the token

> defaultGuard :: a -> b -> GuardResponse
> defaultGuard _ _ = AcceptToken


> completeDefaultExecution :: (WfEngine engine) => engine -> NodeToken -> WfRun a -> IO (WfRun a)
> completeDefaultExecution engine token wf = completeExecution engine token [] wf

completeExecution
  Generates a new token for each output node of the current node of the given
  token.

> completeExecution :: (WfEngine e) => e -> NodeToken -> String -> WfRun a -> IO (WfRun a)
> completeExecution engine token outputArcName wf
>     | hasNoOutputs = return newWf
>     | hasOneOutput = if (firstOutputName == outputArcName)
>                          then do arcToken <- createArcToken engine wf (head outputArcs) token
>                                  completeNodeToken engine token
>                                  acceptToken engine arcToken newWf
>                          else return newWf
>     | otherwise    = do completeNodeToken engine token
>                         split outputArcs newWf
>   where
>     hasNoOutputs        = null outputArcs
>     hasOneOutput        = null $ tail outputArcs
>
>     graph               = wfGraph wf
>     currentNode         = nodeForToken token graph
>     outputArcs          = (graphOutputArcs graph) Map.! (nodeId currentNode)
>
>     firstOutputName     = (arcName.head) outputArcs
>
>     newWf               = removeNodeToken token wf
>
>     split [] wf         = return wf
>     split (arc:arcs) wf = if ( arcName arc == outputArcName)
>                               then do arcToken <- createArcToken engine wf arc token
>                                       newWf <- acceptToken engine arcToken wf
>                                       split arcs newWf
>                               else split arcs wf

acceptToken
  Called when a token arrives at a node. The node is checked to see if it requires
  tokens at all inputs. If it doesn't, the acceptSingle function is called. Otherwise
  it calls acceptJoin.

> acceptToken :: (WfEngine e) => e -> ArcToken -> WfRun a -> IO (WfRun a)
> acceptToken engine token wf
>     | isAcceptSingle = acceptSingle engine token wf
>     | otherwise      = acceptJoin   engine token wf
>   where
>     isAcceptSingle = not $ nodeIsJoin targetNode
>     targetNode     = ((graphNodes.wfGraph) wf) Map.! ((endNodeId.arcForToken) token)

acceptSingle
  Called when a node requires only a single incoming token to activate.
  Moves the token into the node and calls the guard function

> acceptSingle :: (WfEngine e) => e -> ArcToken -> WfRun a -> IO (WfRun a)
> acceptSingle engine token wf =
>   do newToken <- createNodeToken engine wf node [token]
>      completeArcToken engine token
>      acceptWithGuard engine newToken wf { nodeTokens = newToken:(nodeTokens wf) }
>   where
>     graph = wfGraph wf
>     node  = (graphNodes graph) Map.! ((endNodeId.arcForToken) token)

acceptJoin
  Called when a node requires that a token exist at all inputs before activating.
  If the condition is met, joins all the input tokens into a single token in the
  node then calls the guard function.
  If all inputs don't yet have inputs, adds the current token to the workflow
  instance and returns.

> acceptJoin :: (WfEngine e) => e -> ArcToken -> WfRun a -> IO (WfRun a)
> acceptJoin engine token wf@(WfRun runId nodeTypes graph nodeTokens arcTokens userData)
>     | areAllInputsPresent = do newToken <- createNodeToken engine wf targetNode inputTokens
>                                let newWf = WfRun runId nodeTypes graph (newToken:nodeTokens) outputTokenList userData
>                                mapM (completeArcToken engine) inputTokens
>                                acceptWithGuard engine newToken newWf
>     | otherwise           = return $ WfRun runId nodeTypes graph nodeTokens allArcTokens userData
>   where
>     allArcTokens          = token:arcTokens
>     areAllInputsPresent   = length inputTokens == length inputArcs
>
>     fstInputArcToken arc  = ListUtil.firstMatch (\arcToken -> (arcId.arcForToken) arcToken == arcId arc) allArcTokens
>
>     inputTokens           = ListUtil.removeNothings $ map (fstInputArcToken) inputArcs
>
>     targetNodeId          = (endNodeId.arcForToken) token
>     targetNode            = (graphNodes graph) Map.! targetNodeId
>     allInputArcs          = (graphInputArcs graph) Map.! targetNodeId
>     inputArcs             = filter (\arc-> arcName arc == (arcName.arcForToken) token) allInputArcs
>     outputTokenList       = filter (\t -> not $ elem t inputTokens) arcTokens

acceptWithGuard
  This is only called once the node is ready to fire. The given token is now in the node
  and exists in the workflow instance.
  The node guard method is now called and the appropriate action will be taken based on
  what kind of GuardResponse is returned.

> acceptWithGuard :: (WfEngine e) => e -> NodeToken -> WfRun a -> IO (WfRun a)
> acceptWithGuard engine token wf =
>     case (guard token wf) of
>         AcceptToken  -> accept engine token wf
>         DiscardToken -> do completeNodeToken engine token
>                            return $ removeNodeToken token wf
>         SkipNode     -> completeDefaultExecution engine token wf
>     where
>         currentNode  = nodeForToken token (wfGraph wf)
>         guard        = guardFunction  currNodeType
>         accept       = acceptFunction currNodeType
>
>         currNodeType = (nodeTypes wf) Map.! (nodeType currentNode)
