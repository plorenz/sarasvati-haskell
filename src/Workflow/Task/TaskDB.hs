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


module Workflow.Task.TaskDB where

import Data.Dynamic
import Database.HDBC
import Workflow.Loader
import Workflow.DatabaseLoader
import Workflow.Task.Task
import Workflow.Engine
import Workflow.Error

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