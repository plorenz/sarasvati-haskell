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

module Workflow.Sarasvati.MemoryLoader (MemLoader,
                                        newMemLoader,
                                        newSimpleMemLoader,
                                        loadMemWorkflow ) where

import Control.Monad
import Data.IORef
import Data.Map as Map hiding (null, filter, map)

import Text.XML.HaXml.Types

import Workflow.Sarasvati.Engine
import Workflow.Sarasvati.Error
import Workflow.Sarasvati.Loader

data MemLoader = forall resolver . Resolver resolver => MemLoader (IORef Int) resolver

instance Loader (MemLoader) where
    createWorkflow = createMemWorkflow
    createNode     = createMemNode
    createArc      = createMemArc
    importInstance = importMemInstance

newMemLoader :: (Resolver r) => r -> IO MemLoader
newMemLoader resolver =
    do counter <- newIORef 1
       return $ MemLoader counter resolver

newSimpleMemLoader :: String -> Map.Map String (Element -> NodeExtra) -> IO MemLoader
newSimpleMemLoader basePath funcMap = newMemLoader (SimpleFileResolver basePath funcMap)

nextId :: MemLoader -> IO Int
nextId (MemLoader counter _) = atomicModifyIORef counter (\t-> (t + 1, t + 1))

loadMemWorkflow :: MemLoader -> String -> IO (Either String WfGraph)
loadMemWorkflow loader@(MemLoader _ resolver) graphName = catchWf graph
    where
       graph = do xmlWf <- resolveGraphNameToXmlWorkflow resolver graphName
                  graph <- processXmlWorkflow loader xmlWf
                  return $ Right graph

createMemWorkflow :: MemLoader -> a -> IO Int
createMemWorkflow loader _ = nextId loader

createMemNode :: MemLoader -> a -> XmlNode -> IO Node
createMemNode loader _ xmlNode =
    do nodeId <- nextId loader
       return $ xmlNodeToNode nodeId xmlNode

createMemArc :: MemLoader -> a -> String -> Node -> Node -> IO Arc
createMemArc loader _ arcName startNode endNode =
    do arcId <- nextId loader
       return $ Arc arcId arcName (nodeId startNode) (nodeId endNode)

importMemInstance :: MemLoader -> a -> String -> b -> IO ([Node],[Arc])
importMemInstance loader@(MemLoader _ resolver ) _ graphName _ =
    do xmlWf   <- resolveGraphNameToXmlWorkflow resolver graphName
       wfGraph <- processXmlWorkflow loader xmlWf
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
importInstanceArc loader nodeMap arc
    | not (Map.member (startNodeId arc) nodeMap) = error $ "When importing arc, new node not found"
    | not (Map.member (endNodeId arc) nodeMap)   = error $ "When importing arc, new node not found"
    | otherwise = do newArcId <- nextId loader
                     return $ Arc newArcId (arcName arc) newStartNodeId newEndNodeId
    where
        newStartNodeId = nodeId $ nodeMap Map.! (startNodeId arc)
        newEndNodeId   = nodeId $ nodeMap Map.! (endNodeId   arc)