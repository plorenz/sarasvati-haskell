-- Author: Paul Lorenz

-- Loader main. Program arguments are assumed to workflow xml files.
-- Each workflow will be loaded in the database

module Main where

import Database.HDBC
import System
import Text.XML.HaXml.Types
import Workflow.Loaders.XmlToDatabaseLoader
import Workflow.Task.TaskDB
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


