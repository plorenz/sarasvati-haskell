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

module Workflow.Sarasvati.MemoryWfEngine (MemoryWfEngine, newMemoryWfEngine) where

import Data.IORef
import Data.Map as Map hiding (filter, map, null)
import Workflow.Sarasvati.Engine
import Workflow.Sarasvati.TokenUtil as TokenUtil

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
    recordGuardResponse = recordMemGuardResponse
    transactionBoundary = memoryTransactionBoundary
    setProcessAttr      = setMemoryProcessAttr
    removeProcessAttr   = removeMemoryProcessAttr
    setTokenAttr        = setMemoryTokenAttr
    removeTokenAttr     = removeMemoryTokenAttr

createMemoryWfProcess :: (WfEngine engine) => engine ->
                         WfGraph ->
                         Map.Map String (NodeType a) ->
                         Map.Map String (NodeToken -> WfProcess a -> IO Bool) ->
                         a ->
                         Map.Map String String ->
                         IO (WfProcess a)
createMemoryWfProcess _ graph nodeTypes predicates userData attrs = return $ WfProcess 1 nodeTypes graph [] [] attrs Map.empty predicates userData

createMemoryNodeToken :: MemoryWfEngine -> WfProcess a -> Node -> [ArcToken] -> IO (WfProcess a, NodeToken)
createMemoryNodeToken engine process node parentTokens =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       let newToken = NodeToken nextTokenId (nodeId node)
       return (replaceTokenAttrs process newToken (TokenUtil.mergeTokenAttrs process parentTokens), newToken)

createMemoryArcToken :: MemoryWfEngine -> WfProcess a -> Arc -> NodeToken -> IO (WfProcess a, ArcToken)
createMemoryArcToken engine process arc nodeToken =
    do nextTokenId <- atomicModifyIORef (tokenCounter engine) (\t-> (t + 1, t + 1))
       return (process, ArcToken nextTokenId arc nodeToken)

completeMemoryNodeToken :: a -> b -> IO ()
completeMemoryNodeToken _ _ = return ()

completeMemoryArcToken :: a -> b -> IO ()
completeMemoryArcToken _ _ = return ()

recordMemGuardResponse :: a -> b -> c -> IO ()
recordMemGuardResponse _ _ _ = return ()

memoryTransactionBoundary :: MemoryWfEngine -> IO ()
memoryTransactionBoundary _ = return ()

setMemoryProcessAttr :: MemoryWfEngine-> WfProcess a -> String -> String -> IO (WfProcess a)
setMemoryProcessAttr _ process key value = return $ process { attrMap = Map.insert key value (attrMap process) }

removeMemoryProcessAttr :: MemoryWfEngine-> WfProcess a -> String -> IO (WfProcess a)
removeMemoryProcessAttr _ process key = return $ process { attrMap = Map.delete key (attrMap process) }

setMemoryTokenAttr :: MemoryWfEngine-> WfProcess a -> NodeToken -> String -> String -> IO (WfProcess a)
setMemoryTokenAttr _ process nodeToken key value = return newProcess
    where
        newProcess = TokenUtil.setTokenAttr process nodeToken newAttr
        newAttr    = TokenAttr (tokenId nodeToken) key value

removeMemoryTokenAttr :: MemoryWfEngine-> WfProcess a -> NodeToken -> String -> IO (WfProcess a)
removeMemoryTokenAttr _ process nodeToken key = return $ TokenUtil.removeTokenAttr process nodeToken key