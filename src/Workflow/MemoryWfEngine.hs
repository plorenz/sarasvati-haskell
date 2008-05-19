module Workflow.MemoryWfEngine where

import Workflow.Engine
import Data.IORef

data MemoryWfEngine =
    MemoryWfEngine {
       tokenCounter :: IORef Int
    }

instance WfEngine MemoryWfEngine where
    createNodeToken = createMemoryNodeToken
    createArcToken  = createMemoryArcToken


createMemoryNodeToken :: MemoryWfEngine -> a -> Node -> IO NodeToken
createMemoryNodeToken engine _ node =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ NodeToken nextTokenId (nodeId node)

createMemoryArcToken :: MemoryWfEngine -> a -> Arc -> IO ArcToken
createMemoryArcToken engine _ arc =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ ArcToken nextTokenId arc