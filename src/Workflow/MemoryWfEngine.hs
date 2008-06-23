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
    recordGuardResponse = recordMemGuardResponse
    transactionBoundary = memoryTransactionBoundary
    setTokenAttr        = setMemoryTokenAttr
    removeTokenAttr     = removeMemoryTokenAttr

createMemoryWfProcess :: (WfEngine engine) => engine ->
                         WfGraph ->
                         Map.Map String (NodeType a) ->
                         Map.Map String (NodeToken -> WfProcess a -> IO Bool) ->
                         a ->
                         IO (WfProcess a)
createMemoryWfProcess _ graph nodeTypes predicates userData = return $ WfProcess 1 nodeTypes graph [] [] Map.empty predicates userData

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

setMemoryTokenAttr :: MemoryWfEngine-> WfProcess a -> NodeToken -> String -> String -> IO (WfProcess a)
setMemoryTokenAttr _ process nodeToken key value = return newProcess
    where
        newProcess = TokenUtil.setTokenAttr process nodeToken newAttr
        newAttr    = TokenAttr (tokenId nodeToken) key value

removeMemoryTokenAttr :: MemoryWfEngine-> WfProcess a -> NodeToken -> String -> IO (WfProcess a)
removeMemoryTokenAttr _ process nodeToken key = return $ TokenUtil.removeTokenAttr process nodeToken key