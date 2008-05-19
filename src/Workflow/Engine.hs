-- Author: Paul Lorenz

module Workflow.Engine where

import Workflow.EngineTypes
import Workflow.WfError
import qualified Data.Map as Map
import qualified Workflow.Util.ListUtil as ListUtil
import Data.Dynamic
import Control.Monad
import qualified Workflow.GuardLang as GuardLang

makeNodeExtra :: (Typeable a) => a -> NodeExtra
makeNodeExtra extra = NodeExtra $ toDyn extra

tokenAttrs :: WfProcess a -> NodeToken -> [TokenAttr]
tokenAttrs wfProcess token = (tokenAttrMap wfProcess) Map.! (tokenId token)

attrValue :: WfProcess a -> NodeToken -> String -> Maybe String
attrValue process nodeToken key =
    case (attr) of
        [(TokenAttr _ _ value)] -> Just value
        _                       -> Nothing
    where
        attr  = filter (\tokenAttr -> tokenAttrKey tokenAttr == key) (tokenAttrs process nodeToken)

parentToken :: ArcToken -> NodeToken
parentToken (ArcToken _ _ token) = token

replaceTokenAttrs :: WfProcess a -> NodeToken -> [TokenAttr] -> WfProcess a
replaceTokenAttrs process token attrList =
    process { tokenAttrMap = Map.insert (tokenId token) attrList (tokenAttrMap process) }

-- showGraph
--   Print prints a graph

showGraph :: WfGraph -> String
showGraph graph = graphName graph ++ ":\n" ++
                  concatMap (\a->show a ++ "\n") (Map.elems (graphNodes graph)) ++ "\n" ++
                  concatMap (\a->show a ++ "\n") (Map.elems (graphInputArcs graph)) ++ "\n" ++
                  concatMap (\a->show a ++ "\n") (Map.elems (graphOutputArcs graph))

-- graphFromNodesAndArcs
--   Generates a WFGraph from a list of Nodes and Arcs

graphFromArcs :: Int -> String -> [Node] -> [Arc] -> WfGraph
graphFromArcs graphId name nodes arcs = WfGraph graphId name nodeMap inputsMap outputsMap
    where
        nodeMap  = Map.fromList $ zip (map nodeId nodes) nodes

        inputsMap             = Map.fromList $ zip (map nodeId nodes) (map inputArcsForNode nodes)
        inputArcsForNode node = filter (\arc -> endNodeId arc == nodeId node) arcs

        outputsMap = Map.fromList $ zip (map nodeId nodes) (map outputArcsForNode nodes)
        outputArcsForNode node = filter (\arc -> startNodeId arc == nodeId node) arcs

-- getTokenForId
--   Given a token id and a workflow instance gives back the actual token
--   corresponding to that id

getNodeTokenForId :: Int -> WfProcess a -> NodeToken
getNodeTokenForId tokId wf =
  head $ filter (\t -> (tokenId t) == tokId) (nodeTokens wf)

-- Convenience lookup methods for the data pointed to by tokens

nodeForToken :: NodeToken -> WfGraph -> Node
nodeForToken (NodeToken _ nodeId) graph = (graphNodes graph) Map.! nodeId

arcForToken :: ArcToken -> Arc
arcForToken  (ArcToken _ arc _)           = arc

-- startWorkflow
--   Given a workflow definition (WfGraph) and initial userData, gives
--   back a new in progress workflow instance for that definition.

startWorkflow :: (WfEngine engine) =>
                   engine ->
                   Map.Map String (NodeType a) ->
                   Map.Map String (NodeToken -> WfProcess a -> IO Bool) ->
                   WfGraph -> a -> IO ( Either String (WfProcess a))
startWorkflow engine nodeTypes predicates graph userData
    | null startNodes       = return $ Left "Error: Workflow has no start node"
    | length startNodes > 1 = return $ Left "Error: Workflow has more than one start node"
    | otherwise             = do wfRun <- createWfProcess engine graph nodeTypes predicates userData
                                 (wfRun,startToken) <- createNodeToken engine wfRun startNode []
                                 wfRun <- acceptWithGuard engine startToken (wfRun { nodeTokens = [startToken] })
                                 return $ Right wfRun
  where
    startNodes = filter (isStartNode) $ Map.elems (graphNodes graph)
    startNode  = head startNodes
    isStartNode node = (nodeName node == "start") && ((wfDepth.nodeSource) node == 0)

isWfComplete :: WfProcess a -> Bool
isWfComplete process
    | null (nodeTokens process) && null (arcTokens process) = True
    | otherwise                                             = False

-- removeNodeToken
--   Removes the node token from the list of active node tokens in the given process

removeNodeToken :: NodeToken -> WfProcess a -> WfProcess a
removeNodeToken token wf = wf { nodeTokens = ListUtil.removeFirst (\t->t == token) (nodeTokens wf) }

completeDefaultExecution :: (WfEngine engine) => engine -> NodeToken -> WfProcess a -> IO (WfProcess a)
completeDefaultExecution engine token wf = completeExecution engine token [] wf

-- completeExecution
--   Generates a new token for each output node of the current node of the given
--   token.

completeExecution :: (WfEngine e) => e -> NodeToken -> String -> WfProcess a -> IO (WfProcess a)
completeExecution engine token outputArcName wf =
  do completeNodeToken engine token
     foldM (split) newWf outputArcs
  where
    graph        = wfGraph wf
    currentNode  = nodeForToken token graph
    outputArcs   = filter (\arc -> arcName arc == outputArcName ) $
                   (graphOutputArcs graph) Map.! (nodeId currentNode)

    newWf        = removeNodeToken token wf

    split wf arc = do (wf, arcToken) <- createArcToken engine wf arc token
                      acceptToken engine arcToken wf

-- acceptToken
--   Called when a token arrives at a node. The node is checked to see if it requires
--   tokens at all inputs. If it doesn't, the acceptSingle function is called. Otherwise
--   it calls acceptJoin.

acceptToken :: (WfEngine e) => e -> ArcToken -> WfProcess a -> IO (WfProcess a)
acceptToken engine token wf
    | isAcceptSingle = acceptSingle engine token wf
    | otherwise      = acceptJoin   engine token wf
  where
    isAcceptSingle = not $ nodeIsJoin targetNode
    targetNode     = ((graphNodes.wfGraph) wf) Map.! ((endNodeId.arcForToken) token)

-- acceptSingle
--   Called when a node requires only a single incoming token to activate.
--   Moves the token into the node and calls the guard function

acceptSingle :: (WfEngine e) => e -> ArcToken -> WfProcess a -> IO (WfProcess a)
acceptSingle engine token process =
  do (process,newToken) <- createNodeToken engine process node [token]
     completeArcToken engine token
     acceptWithGuard engine newToken process { nodeTokens = newToken:(nodeTokens process) }
  where
    graph = wfGraph process
    node  = (graphNodes graph) Map.! ((endNodeId.arcForToken) token)

-- acceptJoin
--   Called when a node requires that a token exist at all inputs before activating.
--   If the condition is met, joins all the input tokens into a single token in the
--   node then calls the guard function.
--   If all inputs don't yet have inputs, adds the current token to the workflow
--   instance and returns.

acceptJoin :: (WfEngine e) => e -> ArcToken -> WfProcess a -> IO (WfProcess a)
acceptJoin engine token process
    | areAllInputsPresent = do (process,newToken) <- createNodeToken engine process targetNode inputTokens
                               let newProcess = process { nodeTokens = newToken:(nodeTokens process), arcTokens = outputArcTokens }
                               mapM (completeArcToken engine) inputTokens
                               acceptWithGuard engine newToken newProcess
    | otherwise           = return process { arcTokens = allArcTokens }
  where
    allArcTokens          = token:(arcTokens process)
    areAllInputsPresent   = length inputTokens == length inputArcs

    fstInputArcToken arc  = ListUtil.firstMatch (\arcToken -> (arcId.arcForToken) arcToken == arcId arc) allArcTokens

    inputTokens           = ListUtil.removeNothings $ map (fstInputArcToken) inputArcs

    targetNodeId          = (endNodeId.arcForToken) token
    targetNode            = (graphNodes (wfGraph process)) Map.! targetNodeId
    allInputArcs          = (graphInputArcs (wfGraph process)) Map.! targetNodeId
    inputArcs             = filter (\arc-> arcName arc == (arcName.arcForToken) token) allInputArcs
    outputArcTokens       = filter (\t -> not $ elem t inputTokens) (arcTokens process)

-- acceptWithGuard
--   This is only called once the node is ready to fire. The given token is now in the node
--   and exists in the workflow instance.
--   The node guard method is now called and the appropriate action will be taken based on
--   what kind of GuardResponse is returned.

acceptWithGuard :: (WfEngine e) => e -> NodeToken -> WfProcess a -> IO (WfProcess a)
acceptWithGuard engine token wf =
    do guardResponse <- guard token wf
       case guardResponse of
           AcceptToken    -> accept engine token wf
           DiscardToken   -> do completeNodeToken engine token
                                return $ removeNodeToken token wf
           (SkipNode arc) -> completeExecution engine token arc wf
    where
        currentNode  = nodeForToken token (wfGraph wf)
        guard        = guardFunction  currNodeType
        accept       = acceptFunction currNodeType
        currNodeType = (nodeTypes wf) Map.! (nodeType currentNode)

evalGuardLang :: NodeToken -> WfProcess a -> IO GuardResponse
evalGuardLang token wf
    | null guard = return $ AcceptToken
    | otherwise  = evalGuardLangStmt token wf $ GuardLang.evalGuard (GuardLang.lexer guard)
    where
        node  = nodeForToken token (wfGraph wf)
        guard = nodeGuard node

evalGuardLangStmt :: NodeToken -> WfProcess a -> GuardLang.Stmt -> IO GuardResponse
evalGuardLangStmt token wf (GuardLang.StmtResult result)           = return result
evalGuardLangStmt token wf (GuardLang.StmtIF expr ifStmt elseStmt) =
    do result <- evalGuardLangExpr token wf expr
       case result of
          True  -> evalGuardLangStmt token wf ifStmt
          False -> evalGuardLangStmt token wf elseStmt

evalGuardLangExpr :: NodeToken -> WfProcess a -> GuardLang.Expr -> IO Bool
evalGuardLangExpr token wf (GuardLang.ExprSymbol symbol)   = evalGuardLangPred token wf symbol
evalGuardLangExpr token wf (GuardLang.ExprOR  symbol expr) =
    do result <- evalGuardLangPred token wf symbol
       case result of
           True  -> return True
           False -> evalGuardLangExpr token wf expr
evalGuardLangExpr token wf (GuardLang.ExprAND symbol expr) =
    do result <- evalGuardLangPred token wf symbol
       case result of
           True  -> evalGuardLangExpr token wf expr
           False -> return False

evalGuardLangPred :: NodeToken -> WfProcess a -> String -> IO Bool
evalGuardLangPred token wf predicate
    | invalidPredicate = wfError $ "Predicate " ++ predicate ++ " not defined"
    | otherwise        = (predMap Map.! predicate) token wf
    where
        predMap = predicateMap wf
        invalidPredicate = not (Map.member predicate predMap)