-- Author: Paul Lorenz

-- Loader main. Program arguments are assumed to workflow xml files.
-- Each workflow will be loaded in the database

module Main where
import Workflow.Loaders.XmlToDatabaseLoader
import System
import Database.HDBC
import Text.XML.HaXml.Types
import Workflow.Util.XmlUtil as XmlUtil

main :: IO ()
main =
    do args <- getArgs
       case (null args) of
           True  -> putStrLn "No workflow file specified"
           False -> do mapM (load) args
                       return ()

load :: String -> IO ()
load filename =
    do result <- loadWorkflow filename funcMap
       case result of
           Left msg -> do putStrLn $ "Load of " ++ filename ++ " failed: "
                          putStrLn $ "\t" ++msg
           Right _  -> putStrLn $ "Load of " ++ filename ++ " succeeded."
    where
        funcMap = loaderMapWith [ ("task", processTask) ]


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
    do (nodeId, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin "task"
       insertNewNodeTask conn nodeId taskName taskDesc
       return (nodeRefId, nodeName)
    where
        nodeName = readRequiredAttr element "name"
        taskName = readText         element "task-name"
        taskDesc = readText         element "description"

        isJoin = case (readOptionalAttr element "isJoin" "false" ) of
                     "false"   -> False
                     otherwise -> True