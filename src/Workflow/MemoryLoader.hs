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

module Workflow.MemoryLoader where

import           Control.Monad
import           Data.IORef
import qualified Data.Map as Map

import           Workflow.Engine
import           Workflow.Loader

data MemLoader = MemLoader (IORef Int)

instance Loader (MemLoader) where
    createWorkflow = createMemWorkflow
    createNode     = createMemNode
    createArc      = createMemArc
    importInstance = importMemInstance

newMemLoader :: IO MemLoader
newMemLoader =
    do counter <- newIORef 1
       return $ MemLoader counter

nextId :: MemLoader -> IO Int
nextId (MemLoader counter) = atomicModifyIORef counter (\t-> (t + 1, t + 1))

createMemWorkflow :: MemLoader -> XmlWorkflow -> IO Int
createMemWorkflow loader xmlWf = nextId loader

createMemNode :: MemLoader -> Int -> XmlNode -> IO Node
createMemNode loader graphId xmlNode =
    do nodeId <- nextId loader
       return $ xmlNodeToNode nodeId xmlNode extra
    where
        nodeName = xmlNodeName    xmlNode
        isJoin   = xmlNodeIsJoin  xmlNode
        isStart  = xmlNodeIsStart xmlNode
        nodeType = xmlNodeType    xmlNode
        guard    = xmlNodeGuard   xmlNode
        extra    = xmlNodeExtra   xmlNode

createMemArc :: MemLoader -> Int -> String -> Node -> Node -> IO Arc
createMemArc loader graphId arcName startNode endNode =
    do arcId <- nextId loader
       return $ Arc arcId arcName (nodeId startNode) (nodeId endNode)

importMemInstance :: MemLoader -> Int -> String -> IO ([Node],[Arc])
importMemInstance loader@(MemLoader counter) graphId graphName =
    do wfGraph <- resolveWorkflow resolver graphName
       nodeMap <- importInstanceNodes loader (Map.elems (graphNodes wfGraph))
       arcs    <- importInstanceArcs loader nodeMap ((concat.Map.elems) (graphInputArcs wfGraph))
       return (Map.elems nodeMap,arcs)

importInstanceNodes :: MemLoader -> [Node] -> IO (Map.Map Int Node)
importInstanceNodes loader nodes = foldM (importInstanceNode loader) Map.empty nodes

importInstanceNode :: MemLoader -> Map.Map Int Node -> Node -> IO (Map.Map Int Node)
importInstanceNode loader nodeMap node =
    do newNodeId <- nextId loader
       return $ Map.insert (nodeId node) (node {nodeId = newNodeId }) nodeMap

importInstanceArcs :: MemLoader -> Map.Map Int Node -> [Arc] -> IO [Arc]
importInstanceArcs loader nodeMap arcs = mapM (importInstanceArc loader nodeMap) arcs

importInstanceArc :: MemLoader -> Map.Map Int Node -> Arc -> IO Arc
importInstanceArc loader nodeMap arc =
    do newArcId <- nextId loader
       return $ Arc newArcId (arcName arc) newStartNodeId newEndNodeId
    where
        newStartNodeId = nodeId $ nodeMap Map.! (startNodeId arc)
        newEndNodeId   = nodeId $ nodeMap Map.! (endNodeId   arc)