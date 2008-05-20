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

module Workflow.EngineTypes where
import Data.Dynamic
import qualified Data.Map as Map

-- GuardResponse
--   Nodes have guard functions which determine if the accept function when a token
--   arrives and the node is ready to be activated. Guard functions must return a
--   GuardResponse
--
--   AcceptToken  - The token is passed on to the accept function
--   DiscardToken - The token is discarded and the accept function is not called
--   SkipNode     - The accept function is not called. The token is not discarded,
--                  the completeExecution function is called instead.

data GuardResponse = AcceptToken | DiscardToken | SkipNode String
  deriving (Show)

data NodeSource =
    NodeSource {
        wfName     :: String,
        wfVersion  :: String,
        wfInstance :: String,
        wfDepth    :: Int
    }
 deriving (Show, Eq)


-- NodeExtra is a place to store any extra data that a given node may
-- require. The only requirement is that the 'extra data' be a Typeable
-- so it can encapsulated in a Dynamic

data NodeExtra = NoNodeExtra | NodeExtra Dynamic

-- Node
--   Represents a node in a workflow graph.
--
--   Members:
--     nodeId - An integer id, which should be unique. Used for testing equality
--     accept - function which handles incoming tokens.
--
--   Connections between Nodes are represented by Arcs and WFGraph

data Node =
    Node {
        nodeId       :: Int,
        nodeType     :: String,
        nodeName     :: String,
        nodeSource   :: NodeSource,
        nodeIsJoin   :: Bool,
        nodeGuard    :: String,
        nodeExtra    :: NodeExtra
    }

-- NodeType
--   Encapsulates node functionality

data NodeType a =
    NodeType {
        guardFunction  :: (NodeToken -> WfProcess a -> IO GuardResponse),
        acceptFunction :: (WfEngine engine) => (engine -> NodeToken -> WfProcess a -> IO (WfProcess a))
    }


-- Arc
--   An Arc represents an directed edge in a workflow graph.
--   It has an id, a label and two node id endpoints.

data Arc =
    Arc {
        arcId        :: Int,
        arcName      :: String,
        startNodeId  :: Int,
        endNodeId    :: Int
    }
 deriving (Show)


-- Tokens are split into NodeTokens and ArcTokens. NodeTokens are sitting at
-- nodes in the workflow graph while ArcTokens are 'in-transit' and are on
-- Arcs.
--
-- The Token class allows NodeTokens and ArcTokens to share an id lookup function

class Token a where
   tokenId   :: a -> Int

data TokenAttr =
    TokenAttr {
        attrSetId      :: Int,
        tokenAttrKey   :: String,
        tokenAttrValue :: String
    }
  deriving (Show)

-- NodeToken represents tokens which are at node
--   The NodeToken constructor takes three parameters
--   token id :: Int          - The id should be unique among node tokens for this process
--   node  id :: Int          - This should be the id of a node in the graph for this process
data NodeToken = NodeToken Int Int
    deriving (Show)

-- ArcToken represents tokens which are between nodes (on an arc)

data ArcToken = ArcToken Int Arc NodeToken
    deriving (Show)


-- WFGraph
--   Has the set of nodes as well as maps of node input arcs and node output arcs
--   keyed by node id.

data WfGraph =
    WfGraph {
       graphId         :: Int,
       graphName       :: String,
       graphNodes      :: Map.Map Int Node,
       graphInputArcs  :: Map.Map Int [Arc],
       graphOutputArcs :: Map.Map Int [Arc]
    }

-- A WfProcess tracks the current state of the workflow. It has the workflow graph as well
-- as the tokens representing the current state. A slot for user data is also defined.

data WfProcess a =
    WfProcess {
        processId    :: Int,
        nodeTypes    :: Map.Map String (NodeType a),
        wfGraph      :: WfGraph,
        nodeTokens   :: [NodeToken],
        arcTokens    :: [ArcToken],
        tokenAttrMap :: Map.Map Int [TokenAttr],
        predicateMap :: Map.Map String (NodeToken -> WfProcess a -> IO Bool),
        userData     :: a
    }

class WfEngine a where
    createWfProcess     :: a -> WfGraph ->
                                Map.Map String (NodeType b) ->
                                Map.Map String (NodeToken -> WfProcess b -> IO Bool) ->
                                b ->
                                IO (WfProcess b)
    createNodeToken     :: a -> WfProcess b -> Node -> [ArcToken] -> IO (WfProcess b, NodeToken)
    createArcToken      :: a -> WfProcess b -> Arc  -> NodeToken  -> IO (WfProcess b, ArcToken)
    completeNodeToken   :: a -> NodeToken   -> IO ()
    completeArcToken    :: a -> ArcToken    -> IO ()
    transactionBoundary :: a -> IO ()
    setTokenAttr        :: a -> WfProcess b -> NodeToken -> String -> String -> IO (WfProcess b)
    removeTokenAttr     :: a -> WfProcess b -> NodeToken -> String -> IO (WfProcess b)


instance Show (NodeExtra) where
    show NoNodeExtra = "NoNodeExtra"
    show _           = "NodeExtra: Dynamic"


instance Show (Node) where
    show a = "|Node id: " ++ (show.nodeId) a ++ " name: " ++ nodeName a ++
             " depth: " ++ (show.nodeSource) a ++ "|"

instance Token (NodeToken) where
    tokenId (NodeToken tokId _) = tokId

instance Eq (NodeToken) where
    tok1 == tok2 = (tokenId tok1) == (tokenId tok2)

instance Token (ArcToken) where
    tokenId (ArcToken tokId _ _) = tokId

instance Eq (ArcToken) where
    tok1 == tok2 = (tokenId tok1) == (tokenId tok2)

