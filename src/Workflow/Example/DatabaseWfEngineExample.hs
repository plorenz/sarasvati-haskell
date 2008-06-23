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

module Main where

import IO
import Data.Char
import qualified Data.Map as Map
import qualified Workflow.Util.DbUtil as DbUtil
import Database.HDBC
import Database.HDBC.Types
import Workflow.Engine
import Workflow.Error
import Workflow.DatabaseLoader
import Workflow.DatabaseWfEngine
import Workflow.Example.Task
import Workflow.Example.ExampleCommon
import Workflow.Util.DbUtil as DbUtil

main :: IO ()
main =
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
        handleAll = (handleSql handleDbError).(handleWf handleLoadError)

useWorkflow :: [String] -> Int -> IO ()
useWorkflow wfList idx
    | length wfList <= idx = do putStrLn "ERROR: Invalid workflow number"
    | otherwise            = do conn <- DbUtil.openDbConnection
                                graph <- loadLatestGraph conn (wfList !! idx) typeMap
                                putStrLn "Running workflow"
                                putStrLn (show graph)
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
    do conn <- DbUtil.openDbConnection
       withTransaction conn (getWorkflowListFromDb)

getWorkflowListFromDb :: (IConnection conn) => conn -> IO [String]
getWorkflowListFromDb conn =
    do rows <- quickQuery conn sql []
       return $ map (fromSql.head) rows
    where
        sql = "select distinct name from wf_graph order by name asc"

handleLoadError :: WfError -> IO ()
handleLoadError (WfError msg) =
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