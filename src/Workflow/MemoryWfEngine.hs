module Workflow.MemoryWfEngine where

import Data.IORef
import qualified Data.Map as Map
import Workflow.Engine
import qualified Workflow.Util.TokenUtil as TokenUtil

data MemoryWfEngine =
    MemoryWfEngine {
       tokenCounter :: IORef Int
    }

newMemoryWfEngine :: IO MemoryWfEngine
newMemoryWfEngine =
    do tokenCounter <- newIORef 1
       return $ MemoryWfEngine tokenCounter

instance WfEngine MemoryWfEngine where
    createWfProcess     = createMemoryWfProcess
    createNodeToken     = createMemoryNodeToken
    createArcToken      = createMemoryArcToken
    completeNodeToken   = completeMemoryNodeToken
    completeArcToken    = completeMemoryArcToken
    transactionBoundary = memoryTransactionBoundary

createMemoryWfProcess :: MemoryWfEngine -> WfGraph -> Map.Map String (NodeType a) -> a -> IO (WfProcess a)
createMemoryWfProcess _ graph nodeTypes userData = return $ WfProcess 1 nodeTypes graph [] [] userData

createMemoryNodeToken :: MemoryWfEngine -> a -> Node -> [ArcToken] -> IO NodeToken
createMemoryNodeToken engine _ node parentTokens =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ NodeToken nextTokenId (nodeId node) (TokenUtil.mergeTokenAttrs parentTokens)

createMemoryArcToken :: MemoryWfEngine -> a -> Arc -> NodeToken -> IO ArcToken
createMemoryArcToken engine _ arc nodeToken =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return $ ArcToken nextTokenId arc nodeToken

completeMemoryNodeToken :: a -> b -> IO ()
completeMemoryNodeToken _ _ = return ()

completeMemoryArcToken :: a -> b -> IO ()
completeMemoryArcToken _ _ = return ()

memoryTransactionBoundary :: MemoryWfEngine -> IO ()
memoryTransactionBoundary _ = return ()

