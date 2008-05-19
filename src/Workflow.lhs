Author: Paul Lorenz

> module Workflow where
> import qualified Data.Map as Map

NodeType
  Determines if a node will wait for tokens on all inputs before firing, or will fire as soon as
  any token arrives.

  RequireSingle - Node will fire for every token that arrives
  RequireAll    - Node will only fire when there tokens at every input

> data NodeRequireType = RequireSingle | RequireAll
>   deriving (Show)

> nodeTypeFromString "requireSingle" = RequireSingle
> nodeTypeFromString _               = RequireAll

GuardResponse
  Nodes have guard functions which determine if the accept function when a token
  arrives and the node is ready to be activated. Guard functions must return a
  GuardResponse

  AcceptToken  - The token is passed on to the accept function
  DiscardToken - The token is discarded and the accept function is not called
  SkipNode     - The accept function is not called. The toke is not discarded,
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
>  deriving (Show)

Node
  Represents a node in a workflow graph.

  Members:
    nodeId - An integer id, which should be unique. Used for testing equality
    accept - function which handles incoming tokens.

  Connections between Nodes are represented by Arcs and WFGraph

> data Node a =
>     Node {
>         nodeId         :: Int,
>         nodeType       :: String,
>         nodeRefId      :: String,
>         source         :: NodeSource,
>         nodeRequires   :: NodeRequireType,
>         guardFunction  :: (NodeToken -> WfInstance a -> GuardResponse),
>         acceptFunction :: (NodeToken -> WfInstance a -> IO (WfInstance a))
>     }

> instance Show (Node a) where
>     show a = "[Node id: " ++ (show.nodeId) a ++
>              " ref: " ++ nodeRefId a ++
>              " depth: " ++ (show.wfDepth.source) a ++ "]"

Arc
  An Arc represents an directed edge in a workflow graph.
  It has an id, a label and two node id endpoints.

> data Arc =
>     Arc {
>         arcId     :: Int,
>         arcName   :: String,
>         inNodeId  :: Int,
>         outNodeId :: Int
>     }
>  deriving (Show)

Tokens are split into NodeTokens and ArcTokens. NodeTokens are sitting at
nodes in the workflow graph while ArcTokens are 'in-transit' and are on
Arcs.

The Token class allows NodeTokens and ArcTokens to share an id lookup function

> class Token a where
>    tokenId :: a -> [Int]

NodeToken represents tokens which are at node

> data NodeToken = NodeToken [Int] Int
>     deriving (Show)

> instance Token (NodeToken) where
>     tokenId (NodeToken id _ ) = id

> instance Eq (NodeToken) where
>     tok1 == tok2 = (tokenId tok1) == (tokenId tok2)

ArcToken represents tokens which are between nodes (on an arc)

> data ArcToken = ArcToken [Int] Arc
>     deriving (Show)

> instance Token (ArcToken) where
>     tokenId (ArcToken id _) = id


WFGraph
  Has the set of nodes as well as maps of node input arcs and node output arcs
  keyed by node id.

> data WfGraph a =
>     WfGraph {
>        graphNodes      :: Map.Map Int (Node a),
>        graphInputArcs  :: Map.Map Int [Arc],
>        graphOutputArcs :: Map.Map Int [Arc]
>     }

A WfInstance tracks the current state of the workflow. It has the workflow graph as well
as the tokens representing the current state. A slot for user data is also defined.

> data WfInstance a =
>     WfInstance {
>         wfGraph    :: WfGraph a,
>         nodeTokens :: [NodeToken],
>         arcTokens  :: [ArcToken],
>         userData   :: a
>     }

showGraph
  Print prints a graph

> showGraph graph = concatMap (\a->show a ++ "\n") (Map.elems (graphNodes graph)) ++ "\n" ++
>                   concatMap (\a->show a ++ "\n") (Map.elems (graphInputArcs graph)) ++ "\n" ++
>                   concatMap (\a->show a ++ "\n") (Map.elems (graphOutputArcs graph))

graphFromNodesAndArcs
  Generates a WFGraph from a list of NodeArcs

> graphFromArcs nodes arcs = WfGraph nodeMap inputsMap outputsMap
>     where
>         nodeMap  = Map.fromList $ zip (map nodeId nodes) nodes
>
>         inputsMap             = Map.fromList $ zip (map nodeId nodes) (map inputArcsForNode nodes)
>         inputArcsForNode node = filter (\arc -> outNodeId arc == nodeId node) arcs
>
>         outputsMap = Map.fromList $ zip (map nodeId nodes) (map outputArcsForNode nodes)
>         outputArcsForNode node = filter (\arc -> inNodeId arc == nodeId node) arcs

getTokenForId
  Given a token id and a workflow instance gives back the actual token
  corresponding to that id

> getNodeTokenForId id (WfInstance _ nodeTokens _ _) =
>   head $ filter (\t -> (tokenId t) == id) nodeTokens

Convenience lookup methods for the data pointed to by tokens

> nodeForToken (NodeToken _ nodeId) graph = (graphNodes graph) Map.! nodeId
> arcForToken (ArcToken _ arc) = arc

startWorkflow
  Given a workflow definition (WfGraph) and initial userData, gives
  back a new in progress workflow instance for that definition.

> startWorkflow :: WfGraph a -> a -> Either String (IO (WfInstance a))
> startWorkflow graph userData
>     | null startNodes       = Left "Error: Workflow has no start node"
>     | length startNodes > 1 = Left "Error: Workflow has more than one start node"
>     | otherwise             = Right $ acceptWithGuard token wf
>   where
>     startNodes = filter (isStartNode) $ Map.keys (graphNodes graph)
>     startNode  = (graphNodes graph) Map.! (head startNodes)
>     token      = NodeToken [1] (nodeId startNode)
>     wf         = WfInstance graph [token] [] userData
>
>     isStartNode (-1) = True
>     isStartNode _    = False

> isWfComplete (WfInstance graph [] [] userData) = True
> isWfComplete _                                 = False

removeFirst
  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>     | predicate x = xs
>     | otherwise = x : (removeFirst predicate xs)

nextForkId
  Generates the token id for the next token for in the case where we have multiple outputs.
  A token id is a list of integers. For each node which has a single output, the output token
  will have the same id as the input token.
  A node with multiple outputs will add a counter to the tail of the id, incremented for
  each child. This guarantees that each token will have a unique id

  For example, a join with 2 outputs might go
    [1] -> [1,0]             or  [1,2,5] -> [1,2,5,0]
        -> [1,1]                         -> [1,2,5,1]

> nextForkId token counter = (tokenId token) ++ [counter]

removeInputTokens
  Given a list of input arcs, a target node id and a list of arc tokens,
  for each node removes the first token which has the input node as
  its previous node and the target node as its current node

> removeInputTokens []         _          tokenList = tokenList
> removeInputTokens (arc:arcs) targetNodeId tokenList =
>     removeInputTokens arcs targetNodeId $ removeFirst (isInputToken) tokenList
>   where
>     isInputToken tok = (arcId.arcForToken) tok == arcId arc

defaultGuard
  Guard function which always accepts the token

> defaultGuard _ _ = AcceptToken

> completeDefaultExecution token wf = completeExecution token [] wf

completeExecution
  Generates a new token for each output node of the current node of the given
  token.

> completeExecution :: NodeToken -> String -> WfInstance a -> IO (WfInstance a)
> completeExecution token outputArcName wf@(WfInstance graph nodeTokens arcTokens userData)
>   | hasNoOutputs = return newWf
>   | hasOneOutput = if (firstOutputName == outputArcName)
>                        then acceptToken newToken newWf
>                        else return newWf
>   | otherwise    = split outputArcs newWf 0
>   where
>     hasNoOutputs                   = null outputArcs
>     hasOneOutput                   = null $ tail outputArcs
>
>     currentNode                    = nodeForToken token graph
>     outputArcs                     = (graphOutputArcs graph) Map.! (nodeId currentNode)
>
>     firstOutputName                = (arcName.head) outputArcs
>     newToken                       = ArcToken (tokenId token) (head outputArcs)
>     newForkToken arc counter       = ArcToken (nextForkId token counter) arc
>
>     newWf                          = WfInstance graph (removeFirst (\t->t == token) nodeTokens) arcTokens userData
>
>     split [] wf _                  = return wf
>     split (arc:arcs) wf counter = if ( arcName arc == outputArcName)
>                                          then do newWf <- acceptToken (newForkToken arc counter) wf
>                                                  split arcs newWf (counter + 1)
>                                          else split arcs wf (counter)

acceptToken
  Called when a token arrives at a node. The node is checked to see if it requires
  tokens at all inputs. If it doesn't, the acceptSingle function is called. Otherwise
  it calls acceptJoin.

> acceptToken :: ArcToken -> WfInstance a -> IO (WfInstance a)
> acceptToken token wf@(WfInstance graph nodeTokens arcTokens userData)
>     | isAcceptSingle = acceptSingle token wf
>     | otherwise      = acceptJoin   token wf
>   where
>     isAcceptSingle = case (nodeRequires targetNode) of
>                        RequireAll    -> False
>                        RequireSingle -> True
>     targetNode     = (graphNodes graph) Map.! ((outNodeId.arcForToken) token)

acceptSingle
  Called when a node requires only a single incoming token to activate.
  Moves the token into the node and calls the guard function

> acceptSingle :: ArcToken -> WfInstance a -> IO (WfInstance a)
> acceptSingle token wf@(WfInstance graph nodeTokens arcTokens userData) = acceptWithGuard newToken newWf
>   where
>     newToken   = NodeToken (tokenId token) $ (outNodeId.arcForToken) token
>     newWf      = WfInstance graph (newToken:nodeTokens) arcTokens userData

acceptJoin
  Called when a node requires that a token exist at all inputs before activating.
  If the condition is met, joins all the input tokens into a single token in the
  node then calls the guard function.
  If all inputs don't yet have inputs, adds the current token to the workflow
  instance and returns.

> acceptJoin :: ArcToken -> WfInstance a -> IO (WfInstance a)
> acceptJoin token wf@(WfInstance graph nodeTokens arcTokens userData)
>     | areAllInputsPresent = acceptWithGuard newToken newWf
>     | otherwise           = return $ WfInstance graph nodeTokens allArcTokens userData
>   where
>     allArcTokens          = token:arcTokens
>     areAllInputsPresent   = all (inputArcHasToken) inputArcs
>
>     inputArcHasToken arc  = arcName arc /= (arcName.arcForToken) token ||
>                             any (\arcToken -> (arcId.arcForToken) arcToken == arcId arc) allArcTokens
>
>     targetNodeId          = (outNodeId.arcForToken) token
>     inputArcs             = (graphInputArcs graph) Map.! targetNodeId
>     outputTokenList       = removeInputTokens inputArcs targetNodeId arcTokens
>
>     newToken              = NodeToken (tokenId token) targetNodeId
>     newWf                 = WfInstance graph (newToken:nodeTokens) outputTokenList userData

acceptWithGuard
  This is only called once the node is ready to fire. The given token is now in the node
  and exists in the workflow instance.
  The node guard method is now called and the appropriate action will be taken based on
  what kind of GuardResponse is returned.

> acceptWithGuard token wf@(WfInstance graph nodeTokens arcTokens userData) =
>     case (guard token wf) of
>       AcceptToken  -> do -- putStrLn $ "Token accepted into " ++ show currentNode
>                          accept token wf
>       DiscardToken -> return $ WfInstance graph (removeFirst (\t->t == token) nodeTokens) arcTokens userData
>       SkipNode     -> completeExecution token [] wf
>  where
>    currentNode = nodeForToken token graph
>    guard       = guardFunction currentNode
>    accept      = acceptFunction currentNode
