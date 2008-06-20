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

import           Text.XML.HaXml.Types

import           Workflow.Engine
import           Workflow.Loader
import           Workflow.Util.XmlUtil

data MemLoader = forall resolver . Resolver resolver => MemLoader (IORef Int) resolver

instance Loader (MemLoader) where
    loadWorkflow   = loadMemWorkflow
    createWorkflow = createMemWorkflow
    createNode     = createMemNode
    createArc      = createMemArc
    importInstance = importMemInstance

data SimpleResolver =
    SimpleResolver {
        basePath :: String,
        funcMap  :: Map.Map String (Element -> NodeExtra)
    }

instance Resolver SimpleResolver where
    resolveGraphNameToXmlWorkflow resolver name =
        do result <- loadXmlWorkflowFromFile path (funcMap resolver)
           case result of
               Left msg    -> wfLoadError $ "Failed to load '" ++ name ++ "' from file '" ++ path ++
                                           "' because: " ++ msg
               Right xmlWf -> return $ xmlWf
        where
            path = (basePath resolver) ++ name ++ ".wf.xml"

newMemLoader :: (Resolver r) => r -> IO MemLoader
newMemLoader resolver =
    do counter <- newIORef 1
       return $ MemLoader counter resolver

newSimpleMemLoader :: String -> Map.Map String (Element -> NodeExtra) -> IO MemLoader
newSimpleMemLoader basePath funcMap = newMemLoader (SimpleResolver basePath funcMap)

nextId :: MemLoader -> IO Int
nextId (MemLoader counter _) = atomicModifyIORef counter (\t-> (t + 1, t + 1))

loadMemWorkflow :: MemLoader -> String -> IO (Either String WfGraph)
loadMemWorkflow loader@(MemLoader _ resolver) graphName = handleErrors graph
    where
       graph = do xmlWf <- resolveGraphNameToXmlWorkflow resolver graphName
                  graph <- processXmlWorkflow loader xmlWf
                  return $ Right graph
       handleErrors = (handleWfLoad wfErrorToLeft).(handleXml xmlErrorToLeft)


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

importMemInstance :: MemLoader -> a -> String -> IO ([Node],[Arc])
importMemInstance loader@(MemLoader _ resolver ) _ graphName =
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
importInstanceArc loader nodeMap arc =
    do newArcId <- nextId loader
       return $ Arc newArcId (arcName arc) newStartNodeId newEndNodeId
    where
        newStartNodeId = nodeId $ nodeMap Map.! (startNodeId arc)
        newEndNodeId   = nodeId $ nodeMap Map.! (endNodeId   arc)