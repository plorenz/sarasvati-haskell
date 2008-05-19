module Workflow.MemoryWfEngine where

import Workflow.Engine
import Data.IORef
import qualified Data.Map as Map

data MemoryWfEngine =
    MemoryWfEngine {
       tokenCounter :: IORef Int
    }

instance WfEngine MemoryWfEngine where
    createWfRun     = createMemoryWfRun
    createNodeToken = createMemoryNodeToken
    createArcToken  = createMemoryArcToken

createMemoryWfRun :: MemoryWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfRun a)
createMemoryWfRun _ graph nodeTypes userData = return $ WfRun 1 nodeTypes graph [] [] userData

createMemoryNodeToken :: MemoryWfEngine -> a -> Node -> [ArcToken] -> IO NodeToken
createMemoryNodeToken engine _ node _ =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ NodeToken nextTokenId (nodeId node)

createMemoryArcToken :: MemoryWfEngine -> a -> Arc -> NodeToken -> IO ArcToken
createMemoryArcToken engine _ arc _ =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ ArcToken nextTokenId arc
