module Workflow.MemoryWfEngine where

import Workflow.Engine
import Data.IORef
import qualified Data.Map as Map

data MemoryWfEngine =
    MemoryWfEngine {
       tokenCounter :: IORef Int
    }

newMemoryWfEngine :: IO MemoryWfEngine
newMemoryWfEngine =
    do tokenCounter <- newIORef 1
       return $ MemoryWfEngine tokenCounter

instance WfEngine MemoryWfEngine where
    createWfRun         = createMemoryWfRun
    createNodeToken     = createMemoryNodeToken
    createArcToken      = createMemoryArcToken
    completeNodeToken   = completeMemoryNodeToken
    completeArcToken    = completeMemoryArcToken
    transactionBoundary = memoryTransactionBoundary

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

completeMemoryNodeToken :: a -> b -> IO ()
completeMemoryNodeToken _ _ = return ()

completeMemoryArcToken :: a -> b -> IO ()
completeMemoryArcToken _ _ = return ()

memoryTransactionBoundary :: MemoryWfEngine -> IO ()
memoryTransactionBoundary _ = return ()