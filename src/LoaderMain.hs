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

-- Loader main. Program arguments are assumed to workflow xml files.
-- Each workflow will be loaded in the database

module Main where

import Data.Map as Map hiding (null)
import Database.HDBC
import System

import Workflow.Loaders.XmlToDatabaseLoader
import Workflow.Task.Task
import Workflow.Task.TaskDB
import Workflow.Util.DbUtil
import Workflow.Loader
import Workflow.DatabaseLoader



main :: IO ()
main =
    do args <- getArgs
       case (null args) of
           True  -> putStrLn "No workflow file specified"
           False -> do mapM (load) args
                       return ()

load :: String -> IO ()
load filename =
    do conn <- openDbConnection
       result <- loadWfGraphFromFile (DbLoader conn xmlFuncMap dbFuncMap) filename xmlFuncMap
       disconnect conn
       case result of
           Left msg -> do putStrLn $ "Load of " ++ filename ++ " failed: "
                          putStrLn $ "\t" ++msg
           Right _  -> putStrLn $ "Load of " ++ filename ++ " succeeded."
    where
        xmlFuncMap = Map.fromList [ ("task", processTaskElement) ]
        dbFuncMap  = Map.fromList [ ("task", processTask) ]
        loader = DbLoader


