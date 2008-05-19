-- Author: Paul Lorenz

-- Loader main. Program arguments are assumed to workflow xml files.
-- Each workflow will be loaded in the database

module Main where
import Workflow.Loaders.XmlToDatabaseLoader
import System

main =
    do args <- getArgs
       case (null args) of
           True  -> putStrLn "No workflow file specified"
           False -> do results <- mapM (load) args
                       return ()

load filename =
    do result <- loadWorkflow filename
       case result of
           Left msg -> do putStrLn $ "Load of " ++ filename ++ " failed: "
                          putStrLn $ "\t" ++msg
           Right _  -> putStrLn $ "Load of " ++ filename ++ " succeeded."