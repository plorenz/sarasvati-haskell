Author: Paul Lorenz

> module Workflow where
> import Data.Map (Map)
> import Data.Ord
> import qualified Data.Map as Map

NodeType
  Determines if a node will wait for tokens on all inputs before firing, or will fire as soon as
  any token arrives.

  RequireSingle - Node will fire for every token that arrives
  RequireAll    - Node will only fire when there tokens at every input

> data NodeType = RequireSingle | RequireAll
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

  Connections between Nodes are represented by NodeArcs and WFGraph

> data Node a = NullNode
>             | Node {
>                 nodeId         :: Int,
>                 nodeRefId      :: String,
>                 source         :: NodeSource,
>                 nodeType       :: NodeType,
>                 guardFunction  :: (Token a -> WfInstance a -> GuardResponse),
>                 acceptFunction :: (Token a -> WfInstance a -> IO (WfInstance a))
>               }

> instance Eq (Node a) where
>     NullNode  == NullNode = True
>     NullNode  == _        = False
>     _         == NullNode = False
>     node1     == node2    = (nodeId node1) == (nodeId node2)

> instance Show (Node a) where
>     show NullNode = "[Node: NullNode]"
>     show a        = "[Node id: " ++ (show.nodeId) a ++ " ref: " ++ nodeRefId a ++ " depth: " ++ (show.wfDepth.source) a ++ "]"

> data Arc =
>     Arc {
>         arcId     :: Int,
>         arcName   :: String,
>         inNodeId  :: Int,
>         outNodeId :: Int
>     }
>  deriving (Show)

> arcInNode  graph arc = (graphNodes graph) Map.! (inNodeId arc)

> arcOutNode graph arc = (graphNodes graph) Map.! (outNodeId arc)

Token
  The set of current tokens gives the current state of the workflow.

  Members:
    tokenId: A list of int, giving a unique id across all tokens
    tokenArcName: The name of arc we are traversing or just traversed
    prevNode: The input node the token came from/is coming from
    currNode: If the token is being processed by a node, this will be set
              to that node. Otherwise it will be set to NullNode
    nextNode: If the token is between nodes, this will be set to the node
              it is going to. Otherwise it will be set to NullNode

> data Token a =
>     Token {
>         tokenId       :: [Int],
>         tokenArcName  :: String,
>         prevNode      :: Node a,
>         currNode      :: Node a,
>         nextNode      :: Node a
>     }
>     deriving (Show)

> instance Eq (Token a) where
>     t1 == t2 = (tokenId t1) == (tokenId t2)

> defaultArc = ""

WFGraph
  This is just a container for NodeArcs, which can be queried
  for the inputs and outputs of a given node

> data WfGraph a =
>     WfGraph {
>        graphNodes      :: Map Int (Node a),
>        graphInputArcs  :: Map Int [Arc],
>        graphOutputArcs :: Map Int [Arc]
>     }

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

> data WfInstance a =
>     WfInstance {
>         wfGraph   :: WfGraph a,
>         tokenList :: [Token a],
>         userData  :: a
>     }

inputs
  Returns the Nodes which are inputs to the given node

> inputs graph node = (graphInputArcs graph) Map.! (nodeId node)

outputs
  Returns the Nodes which are outputs of the given node

> outputs graph node = (graphOutputArcs graph) Map.! (nodeId node)

getTokenForId
  Given a token id and a workflow instance gives back the actual token
  corresponding to that id

> getTokenForId id (WfInstance graph tokenList userData) =
>   head $ filter (\t -> (tokenId t) == id) tokenList

getNodeForId
  Given a node id and a workflow instance gives back the actual node
  corresponding to that id

> nodeForId nodeId (WfInstance graph tokenList userData) = (graphNodes graph) Map.! nodeId

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
>     token      = Token [1] defaultArc NullNode startNode NullNode
>     wf         = WfInstance graph [] userData
>
>     isStartNode (-1) = True
>     isStartNode _    = False

> isWfComplete (WfInstance graph [] userData) = True
> isWfComplete _                              = False

removeFirst
  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>   | predicate x = xs
>   | otherwise = x : (removeFirst predicate xs)

nextForkId
  Generates the token id for the next token for in the case where we have multiple outputs.
  A token id is a list of integers. For each node which has a single output, the output token
  will have the same id as the input token.
  A node with multiple outputs will add a counter to the tail of the id, incremented for
  each child. This guarantees that each token will have a unique id

  For example, a join with 2 outputs might go
    [1] -> [1,0]             or  [1,2,5] -> [1,2,5,0]
        -> [1,1]                         -> [1,2,5,1]

> nextForkId (Token tid _ _ _ _) counter = tid ++ [counter]

removeInputTokens
  Given a list of input nodes, a target node and a list of tokens,
  for each node removes the first token which has the input node as
  its previous node and the target node as its current node

> removeInputTokens []     _          tokenList = tokenList
> removeInputTokens (arc:arcs) targetNode tokenList =
>     removeInputTokens arcs targetNode $ removeFirst (isInputToken) tokenList
>   where
>     isInputToken tok = (nodeId.prevNode) tok == inNodeId arc &&
>                        nextNode tok == targetNode

defaultGuard
  Guard function which always accepts the token

> defaultGuard token wf = AcceptToken

> completeDefaultExecution token wf = completeExecution token [] wf

completeExecution
  Generates a new token for each output node of the current node of the given
  token.

> completeExecution :: Token a -> String -> WfInstance a -> IO (WfInstance a)
> completeExecution token outputArcName wf@(WfInstance graph tokenList userData)
>   | hasNoOutputs = return newWf
>   | hasOneOutput = if (firstOutputName == outputArcName)
>                        then acceptToken newToken newWf
>                        else return newWf
>   | otherwise    = split outputArcs newWf 0
>   where
>     hasNoOutputs                   = null outputArcs
>     hasOneOutput                   = null $ tail outputArcs
>
>     currentNode                    = currNode token
>     outputArcs                     = outputs graph currentNode
>
>     firstOutputName                = (arcName.head) outputArcs
>     firstOutputNode                = ((arcOutNode graph).head) outputArcs
>     newToken                       = Token (tokenId token) outputArcName currentNode NullNode firstOutputNode
>     newForkToken nextNode counter  = Token (nextForkId token counter) outputArcName currentNode NullNode nextNode
>
>     newWf                          = WfInstance graph (removeFirst (\t->t == token) tokenList) userData
>
>     split [] wf _                  = return wf
>     split (arc:arcs) wf counter = if ( arcName arc == outputArcName)
>                                          then do newWf <- acceptToken (newForkToken (arcOutNode graph arc) counter) wf
>                                                  split arcs newWf (counter + 1)
>                                          else split arcs wf (counter)

acceptToken
  Called when a token arrives at a node. The node is checked to see if it requires
  tokens at all inputs. If it doesn't, the acceptSingle function is called. Otherwise
  it calls acceptJoin.

> acceptToken :: Token a -> WfInstance a -> IO (WfInstance a)
> acceptToken token wf@(WfInstance graph tokenList userData)
>     | isAcceptSingle = acceptSingle token wf
>     | otherwise      = acceptJoin   token wf
>   where
>     isAcceptSingle = case (nodeType targetNode) of
>                        RequireAll    -> False
>                        RequireSingle -> True
>     targetNode     = nextNode token

acceptSingle
  Called when a node requires only a single incoming token to activate.
  Moves the token into the node and calls the guard function

> acceptSingle :: Token a -> WfInstance a -> IO (WfInstance a)
> acceptSingle token wf@(WfInstance graph tokenList userData) = acceptWithGuard newToken newWf
>   where
>     targetNode = nextNode token
>     newToken   = Token (tokenId token) (tokenArcName token) (prevNode token) (nextNode token) NullNode
>     newWf      = WfInstance graph tokenList userData

acceptJoin
  Called when a node requires that a token exist at all inputs before activating.
  If the condition is met, joins all the input tokens into a single token in the
  node then calls the guard function.
  If all inputs don't yet have inputs, adds the current token to the workflow
  instance and returns.

> acceptJoin :: Token a -> WfInstance a -> IO (WfInstance a)
> acceptJoin token wf@(WfInstance graph tokenList userData)
>     | areAllInputsPresent = acceptWithGuard newToken newWf
>     | otherwise           = return $ WfInstance graph (token:tokenList) userData
>   where
>     areAllInputsPresent          = all (inputHasToken (token:tokenList)) inputArcs
>
>     inputHasToken []         arc = False
>     inputHasToken (tok:rest) arc = arcName arc /= tokenArcName token ||
>                                    ( nextNode tok == targetNode &&
>                                      (nodeId.prevNode) tok == inNodeId arc &&
>                                      tokenArcName  tok == tokenArcName token ) ||
>                                      inputHasToken rest arc
>
>     targetNode                   = nextNode token
>     inputArcs                    = inputs graph targetNode
>     outputTokenList              = removeInputTokens inputArcs targetNode tokenList
>
>     newToken                     = Token (tokenId token) (tokenArcName token) (prevNode token) (nextNode token) NullNode
>     newWf                        = WfInstance graph (newToken:outputTokenList) userData

acceptWithGuard
  This is only called once the node is ready to fire. The given token is now in the node
  and exists in the workflow instance.
  The node guard method is now called and the appropriate action will be taken based on
  what kind of GuardResponse is returned.

> acceptWithGuard token wf@(WfInstance graph tokenList userData) =
>     case (guard token wf) of
>       AcceptToken  -> do -- putStrLn $ "Token accepted into " ++ show currentNode
>                          accept token wf
>       DiscardToken -> return $ WfInstance graph (removeFirst (\t->t == token) tokenList) userData
>       SkipNode     -> completeExecution token [] wf
>  where
>    currentNode = currNode token
>    guard       = guardFunction currentNode
>    accept      = acceptFunction currentNode
