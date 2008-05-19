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
       return $ NodeToken nextTokenId (nodeId node) (tokenState parentTokens)

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

parentAttr = (tokenAttr.parentToken)

tokenState [] = []
tokenState [token] = parentAttr token
tokenState (x:xs)  = []

mergeAttrs :: (Eq k) => [(k,v)] -> [(k,v)] -> [(k,v)]
mergeAttrs list1 list2 = foldr (mergeAttr) list2 list1

mergeAttr :: (Eq k) => [(k,v)] -> (k,v) -> [(k,v)]
mergeAttr []             (insKey,insVal) = [(insKey,insVal)]
mergeAttr ((key,val):xs) (insKey,insVal)
   | insKey == key = (key,val) : xs
   | otherwise     = (key,val) : mergeAttr xs (insKey, insVal)
