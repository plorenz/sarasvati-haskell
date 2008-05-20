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

--Author: Paul Lorenz

module Workflow.UI.ConsoleDatabaseUI where

import IO
import Data.Char
import Workflow.WfError
import Workflow.Loaders.DatabaseToEngineLoader
import Workflow.Loaders.LoadError
import qualified Data.Map as Map
import qualified Workflow.Util.DbUtil as DbUtil
import Database.HDBC
import Database.HDBC.Types
import Workflow.EngineTypes
import Workflow.Engine
import Workflow.DatabaseWfEngine
import Random
import Workflow.Task.Task
import Workflow.Task.TaskDB
import Workflow.UI.ConsoleCommon
import Workflow.Util.DbUtil as DbUtil

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
         then handleAll (useWorkflow wfList (((read wf)::Int) - 1))
         else do putStrLn $ "ERROR: " ++ wf ++ " is not a valid workflow"
       selectWorkflow wfList
    where
        handleAll = (handleSql handleDbError).(handleLoad handleLoadError)

useWorkflow :: [String] -> Int -> IO ()
useWorkflow wfList idx
    | length wfList <= idx = do putStrLn "ERROR: Invalid workflow number"
    | otherwise            = do conn <- DbUtil.openDbConnection
                                graph <- loadGraph conn (wfList !! idx) typeMap
                                putStrLn "Running workflow"
                                putStrLn (showGraph graph)
                                runWorkflow graph
   where
       typeMap = Map.fromList [ ("task", loadTask) ]

runWorkflow :: WfGraph -> IO ()
runWorkflow graph =
    do conn <- DbUtil.openDbConnection
       let engine = DatabaseWfEngine conn
       result <- startWorkflow engine nodeTypeMap predMap graph []
       case (result) of
           Left msg -> do rollback conn
                          putStrLn msg
           Right wf -> do commit conn
                          processTasks engine wf

nodeTypeMap :: Map.Map String (NodeType [Task])
nodeTypeMap = Map.fromList
                [ ( "start", NodeType evalGuardLang completeDefaultExecution ),
                  ( "node",  NodeType evalGuardLang completeDefaultExecution ),
                  ( "task",  NodeType evalGuardLang acceptAndCreateTask ),
                  ( "init",  NodeType evalGuardLang acceptInit ),
                  ( "dump",  NodeType evalGuardLang acceptDump ) ]

predMap :: Map.Map String (NodeToken -> WfProcess a -> IO Bool)
predMap = Map.fromList [ ("isRandOdd", predIsRandOdd),
                         ("isRandEven", predIsRandEven),
                         ("isTenthIteration", predIsTenthIteration) ]

acceptInit :: (WfEngine engine) => engine -> NodeToken -> WfProcess a -> IO (WfProcess a)
acceptInit engine token process =
    do process <- setTokenAttr engine process token "iter" (show newVal)
       nextRand <- getStdRandom (randomR (1,2))::(IO Int)
       putStrLn $ "Next random: " ++ (show nextRand)
       process <- setTokenAttr engine process token "rand" (show nextRand)
       completeDefaultExecution engine token process
    where
        newVal = case (attrValue process token "iter") of
                     Nothing -> 0
                     Just x  -> (read x::Int) + 1

acceptDump :: (WfEngine engine) => engine -> NodeToken -> WfProcess a -> IO (WfProcess a)
acceptDump engine token process =
    do putStrLn $ "Accepted into " ++ (nodeName node)
       completeDefaultExecution engine token process
    where
       node = nodeForToken token (wfGraph process)

predIsRandOdd :: NodeToken -> WfProcess a -> IO Bool
predIsRandOdd token process = return isOdd
    where
       randVal = attrValueReq process token "rand"
       isOdd   = (read randVal::Int) `mod` 2 == 0

predIsRandEven :: NodeToken -> WfProcess a -> IO Bool
predIsRandEven token process =
    do result <- predIsRandOdd token process
       return $ not result

predIsTenthIteration :: NodeToken -> WfProcess a -> IO Bool
predIsTenthIteration token process = return isTenth
    where
       iterVal = attrValueReq process token "iter"
       isTenth = (read iterVal::Int) >= 10

getWorkflowList :: IO [String]
getWorkflowList =
    do conn <- DbUtil.openDbConnection
       withTransaction conn (getWorkflowListFromDb)

getWorkflowListFromDb :: (IConnection conn) => conn -> IO [String]
getWorkflowListFromDb conn =
    do rows <- quickQuery conn sql []
       return $ map (fromSql.head) rows
    where
        sql = "select distinct name from wf_graph order by name asc"

handleLoadError :: LoadException -> IO ()
handleLoadError (LoadException msg) =
    do putStrLn msg
       return $ ()

handleWorkflowError :: WfException -> IO ()
handleWorkflowError (WfException msg) =
    do putStrLn msg
       return $ ()

handleDbError :: SqlError -> IO ()
handleDbError sqlError =
    do putStrLn msg
       return ()
    where
       msg = "Database error: " ++ (seErrorMsg sqlError)