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

-- Author: Paul Lorenz

module Workflow.UI.ConsoleXmlFileUI where

import Workflow.Error
import Workflow.Engine
import Workflow.Task.Task
import IO
import Data.Char
import System.Directory
import Workflow.MemoryLoader
import Workflow.Task.Task
import qualified Data.Map as Map
import Workflow.UI.ConsoleCommon
import Workflow.MemoryWfEngine

consoleMain :: IO ()
consoleMain =
    do hSetBuffering stdout NoBuffering
       wfList <- getWorkflowList
       selectWorkflow wfList

selectWorkflow :: [String] -> IO ()
selectWorkflow wfList =
    do putStrLn "\n-=Available workflows=-"
       showWorkflows wfList 1
       putStr "\nSelect workflow: "
       wf <- getLine
       if ((not $ null wf) && all (isDigit) wf)
         then do result <- catchWf $ useWorkflow wfList (((read wf)::Int) - 1)
                 case (result) of
                     Left msg -> putStrLn $ "Error: " ++ msg
                     Right () -> return ()
         else do putStrLn $ "ERROR: " ++ wf ++ " is not a valid workflow"
       selectWorkflow wfList

useWorkflow :: [String] -> Int -> IO (Either String ())
useWorkflow wfList idx
    | length wfList <= idx = return $ Left "ERROR: Invalid workflow number"
    | otherwise            = do loader <- newSimpleMemLoader "/home/paul/workspace/wf-haskell/common/test-wf/" funcMap
                                result <- loadMemWorkflow loader (wfList !! idx)
                                case (result) of
                                    Left msg -> return $ Left $ "ERROR: Could not load workflow: " ++ msg
                                    Right graph -> do putStrLn "Running workflow"
                                                      putStrLn (show graph)
                                                      runWorkflow graph
                                                      return $ Right ()
   where
       funcMap = Map.fromList [ ("task", processTaskElement) ]

runWorkflow :: WfGraph -> IO ()
runWorkflow graph =
    do engine <- newMemoryWfEngine
       result <- startWorkflow engine nodeTypeMap predMap graph []
       case (result) of
           Left msg -> putStrLn msg
           Right wf -> processTasks engine wf

nodeTypeMap :: Map.Map String (NodeType [Task])
nodeTypeMap = Map.fromList
                [ ( "node",  NodeType evalGuardLang completeDefaultExecution ),
                  ( "task",  NodeType evalGuardLang acceptAndCreateTask ),
                  ( "init",  NodeType evalGuardLang acceptInit ),
                  ( "dump",  NodeType evalGuardLang acceptDump ) ]

predMap :: Map.Map String (NodeToken -> WfProcess a -> IO Bool)
predMap = Map.fromList [ ("isRandOdd", predIsRandOdd),
                         ("isRandEven", predIsRandEven),
                         ("isTenthIteration", predIsTenthIteration) ]

getWorkflowList :: IO [String]
getWorkflowList =
    do fileList <- getDirectoryContents wfDir
       return $ (stripExt.filterWfs) fileList
    where
        wfDir = "/home/paul/workspace/wf-haskell/common/test-wf/"
        filterWfs = (filter (hasExtension ".wf.xml"))
        stripExt = map (reverse.(drop 7).reverse)

hasExtension :: String -> String -> Bool
hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)