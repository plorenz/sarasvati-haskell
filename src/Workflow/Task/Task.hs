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


module Workflow.Task.Task where

import Workflow.Engine
import qualified Data.Map as Map
import Data.Dynamic

data TaskDef =
    TaskDef {
       taskDefName :: String,
       taskDefDesc :: String
   }
   deriving (Typeable)

data TaskState = Open | Complete | Rejected
 deriving (Show,Eq)

data Task =
  Task {
    getTokId       :: Int,
    taskId         :: String,
    taskName       :: String,
    taskDesc       :: String,
    taskState      :: TaskState,
    taskRejectable :: Bool
  }

showTaskList :: [Task] -> IO ()
showTaskList tasks =
  do putStrLn "Tasks:"
     if (null tasks)
       then putStrLn "  No tasks to display"
       else showTasks tasks 1

showTasks :: [Task] -> Int -> IO ()
showTasks [] _ = do return ()
showTasks (task:rest) counter =
 do putStrLn $ show counter ++ ": " ++ (taskName task) ++ " - " ++ show (taskState task)
    showTasks rest (counter + 1)

acceptAndCreateTask :: (WfEngine e) => e -> NodeToken -> WfProcess [Task] -> IO (WfProcess [Task])
acceptAndCreateTask engine token process =
    do process <- case (attrValue process token key) of
                      Just val -> setTokenAttr engine process token key $ show $ (1 + (read val)::Int)
                      Nothing  -> setTokenAttr engine process token key "1"
       return process { userData = task: (userData process) }
    where
        task = newTask process token
        key  = taskName task

newTask :: WfProcess [Task] -> NodeToken -> Task
newTask wf token = Task (tokenId token) (show theNodeId) taskName taskDesc Open hasReject
    where
        node      = nodeForToken token (wfGraph wf)
        theNodeId = nodeId node
        taskDef   = case (nodeExtra node) of
                         NodeExtra dyn -> fromDyn dyn (TaskDef "default" "default")
                         NoNodeExtra   -> TaskDef "default" "default"
        taskName  = taskDefName taskDef
        taskDesc  = taskDefDesc taskDef
        hasReject = not.null $ filter (\arc -> arcName arc =="reject") $ ((graphOutputArcs.wfGraph) wf) Map.! theNodeId

closeTask :: Task -> WfProcess [Task] -> TaskState -> WfProcess [Task]
closeTask task wf newState = wf { userData = newTaskList }
  where
    newTaskList = map (closeIfMatches) (userData wf)
    closeIfMatches t = if (taskId t == taskId task && (taskState t == Open))
                           then t { taskState=newState }
                           else t

completeTask :: (WfEngine e) => e -> Task -> WfProcess [Task] -> IO (WfProcess [Task])
completeTask engine task wf = completeExecution engine token [] (closeTask task wf Complete)
  where
    token = getNodeTokenForId (getTokId task) wf

rejectTask :: (WfEngine e) => e -> Task -> WfProcess [Task] -> IO (WfProcess [Task])
rejectTask engine task wf = completeExecution engine token "reject" (closeTask task wf Rejected)
  where
    token = getNodeTokenForId (getTokId task) wf