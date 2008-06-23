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


module Workflow.Example.Task where

import Data.Dynamic
import Data.Map as Map hiding (null, filter, map)

import Database.HDBC

import Text.XML.HaXml.Types

import Workflow.DatabaseLoader
import Workflow.Error
import Workflow.Engine
import Workflow.Loader
import Workflow.Util.XmlUtil


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

processTaskElement :: Element -> NodeExtra
processTaskElement element = makeNodeExtra $ TaskDef name description
    where
        taskDefElem = getChildNamed element "ex:task-def"
        name        = readText taskDefElem  "ex:task-name"
        description = readText taskDefElem  "ex:description"

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

---------------------------------------------------------------------------------------------------
--              Loading Functions                                                                --
---------------------------------------------------------------------------------------------------

loadTask :: (IConnection conn) => conn -> Int -> IO NodeExtra
loadTask conn nodeId =
    do rows <- quickQuery conn sql [toSql nodeId]
       case (null rows) of
           True  -> wfError $ "No record for wf_node_task found for node with id: " ++ (show nodeId)
           False -> return $ finishTaskLoad (head rows)
    where
        sql = "select name, description from wf_node_task where id = ?"

finishTaskLoad :: [SqlValue] -> NodeExtra
finishTaskLoad row = makeNodeExtra $ TaskDef name description
    where
        name        = fromSql (row !! 0)
        description = fromSql (row !! 1)

insertTaskDef :: DbLoader -> Int -> XmlNode -> IO ()
insertTaskDef (DbLoader conn _ _) nodeId xmlNode =
    do run conn sql [toSql nodeId,
                     toSql (taskDefName taskDef),
                     toSql (taskDefDesc taskDef)]
       return ()
    where
        sql     = "insert into wf_node_task (id, name, description) values ( ?, ?, ? )"
        taskDef = case (xmlNodeExtra xmlNode) of
                      NodeExtra dyn -> fromDyn dyn (TaskDef "default" "default")
                      NoNodeExtra   -> TaskDef "default" "default"