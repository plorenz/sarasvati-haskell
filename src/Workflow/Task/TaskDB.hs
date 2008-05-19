
module Workflow.Task.TaskDB where

import Database.HDBC
import Text.XML.HaXml.Types
import Workflow.Loaders.LoadError
import Workflow.Loaders.XmlToDatabaseLoader
import Workflow.Task.Task
import Workflow.Util.XmlUtil
import Workflow.Util.ListUtil
import Workflow.EngineTypes
import Workflow.Engine

loadTask :: (IConnection conn) => conn -> Int -> IO NodeExtra
loadTask conn nodeId =
    do rows <- quickQuery conn sql [toSql nodeId]
       case (null rows) of
           True  -> loadError $ "No record for wf_node_task found for node with id: " ++ (show nodeId)
           False -> return $ finishTaskLoad (head rows)
    where
        sql = "select name, description from wf_node_task where id = ?"

finishTaskLoad :: [SqlValue] -> NodeExtra
finishTaskLoad row = makeNodeExtra $ TaskDef name description
    where
        name        = fromSql (row !! 0)
        description = fromSql (row !! 1)

insertNewNodeTask :: (IConnection a) => a -> Int -> String -> String -> IO ()
insertNewNodeTask conn nodeId taskName taskDesc =
    do run conn sql [toSql nodeId,
                     toSql taskName,
                     toSql taskDesc]
       return ()
    where
        sql = "insert into wf_node_task (id, name, description) values ( ?, ?, ? )"

processTask :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
processTask element conn graphId =
    do (nodeId, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin "task" guard
       insertNewNodeTask conn nodeId taskName taskDesc
       return (nodeRefId, nodeName)
    where
        nodeName = readRequiredAttr element "name"
        taskName = readText         element "task-name"
        taskDesc = readText         element "description"
        guard    = trim $ readText  element "guard"

        isJoin = case (readOptionalAttr element "isJoin" "false" ) of
                     "false" -> False
                     _       -> True