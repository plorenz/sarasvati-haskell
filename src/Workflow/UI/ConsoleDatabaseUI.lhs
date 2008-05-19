Author: Paul Lorenz

> module Workflow.UI.ConsoleDatabaseUI where

> import Workflow.Engine
> import Workflow.Task.Task
> import Workflow.Task.TaskDB
> import IO
> import Data.Char
> import Workflow.Loaders.DatabaseToEngineLoader
> import Workflow.Loaders.LoadError
> import qualified Data.Map as Map
> import qualified Workflow.Util.DbUtil as DbUtil
> import Database.HDBC

> handleTask :: Task -> WfInstance [Task] -> IO (WfInstance [Task])
> handleTask task wf =
>     do putStrLn $ "Task name: " ++ (taskName task)
>        putStrLn $ "Task desc: " ++ (taskDesc task)
>        putStrLn $ "Task state: " ++ show (taskState task)
>        case (taskState task) of
>            Open -> do putStr prompt
>                       response <- getLine
>                       case (response) of
>                           "1" -> do newWf <- completeTask task wf
>                                     putStrLn "Task Completed"
>                                     return newWf
>                           "2" -> if (rejectable)
>                                      then do newWf <- rejectTask task wf
>                                              putStrLn "Task Rejected"
>                                              return newWf
>                                      else do putStrLn "Ok. Leaving open"
>                                              return wf
>                           _   -> do putStrLn "Ok. Leaving open"
>                                     return wf
>            Complete -> return wf
>            Rejected -> return wf
>     where
>         rejectable = taskRejectable task
>         prompt = case (rejectable) of
>                      True ->  "1. Complete task\n2. Reject task\n3. Leave task open\nAction: "
>                      False -> "1. Complete task\n2. Leave task open\nAction: "

> getTask :: Integer -> [Task] -> Either String Task
> getTask _ [] = Left "Invalid task number"
> getTask taskNumber (first:rest)
>     | taskNumber <  1 = Left "Invalid task number"
>     | taskNumber == 1 = Right first
>     | otherwise       = getTask (taskNumber - 1) rest

> showTokens :: (Show a) => [a] -> IO ()
> showTokens []     = return ()
> showTokens (x:xs) = do putStrLn (show x)
>                        showTokens xs

> processTasks :: WfInstance [Task] -> IO ()
> processTasks    (WfInstance _ _ [] [] _    ) = putStrLn "Workflow complete!"
> processTasks wf@(WfInstance _ _ _  _  tasks) =
>     do putStrLn ""
>        showTaskList tasks
>        putStr "> "
>        cmd <- getLine
>        case (getCmdType cmd) of
>            ShowTokenCmd -> do putStrLn "Node Tokens"
>                               showTokens (nodeTokens wf)
>                               putStrLn "\nArc Tokens"
>                               showTokens (arcTokens wf)
>                               processTasks wf
>            TaskCmd ->
>                case (getTask ((read cmd)::Integer) tasks) of
>                    Left msg -> do putStrLn msg
>                                   processTasks wf
>                    Right task -> do newWf <- handleTask task wf
>                                     processTasks newWf
>            BadCmd -> do putStrLn $ cmd ++ " is not a valid command or task entry"
>                         processTasks wf
>            NoCmd  -> processTasks wf

> data CmdType = ShowTokenCmd | TaskCmd | BadCmd | NoCmd

> getCmdType :: String -> CmdType
> getCmdType input
>     | null input                   = NoCmd
>     | (map (toUpper) input) == "T" = ShowTokenCmd
>     | all (isDigit) input          = TaskCmd
>     | otherwise                    = BadCmd

> consoleMain :: IO ()
> consoleMain =
>     do hSetBuffering stdout NoBuffering
>        wfList <- getWorkflowList
>        selectWorkflow wfList

> selectWorkflow :: [String] -> IO ()
> selectWorkflow wfList =
>     do putStrLn "\n-=Available workflows=-"
>        showWorkflows wfList 1
>        putStr "\nSelect workflow: "
>        wf <- getLine
>        if ((not $ null wf) && all (isDigit) wf)
>          then handleAll (useWorkflow wfList (((read wf)::Int) - 1))
>          else do putStrLn $ "ERROR: " ++ wf ++ " is not a valid workflow"
>        selectWorkflow wfList
>     where
>         handleAll = (handleSql handleDbError).(handleLoad handleLoadError)

> useWorkflow :: [String] -> Int -> IO ()
> useWorkflow wfList idx
>     | length wfList <= idx = do putStrLn "ERROR: Invalid workflow number"
>     | otherwise            = do conn <- DbUtil.openDbConnection
>                                 graph <- loadGraph conn (wfList !! idx) typeMap
>                                 putStrLn "Running workflow"
>                                 putStrLn (showGraph graph)
>                                 runWorkflow graph
>    where
>        typeMap = Map.fromList [ ("task", loadTask) ]

> runWorkflow :: WfGraph -> IO ()
> runWorkflow graph =
>     case (startWorkflow nodeTypeMap graph []) of
>            Left msg -> putStrLn msg
>            Right wfInstanceIO -> do wf <- wfInstanceIO
>                                     processTasks wf

> nodeTypeMap :: Map.Map String (NodeType [Task])
> nodeTypeMap = Map.fromList
>                 [ ( "start", NodeType defaultGuard completeDefaultExecution ),
>                   ( "node",  NodeType defaultGuard completeDefaultExecution ),
>                   ( "task",  NodeType defaultGuard acceptAndCreateTask ) ]

> showWorkflows :: [String] -> Int -> IO ()
> showWorkflows []        _       = return ()
> showWorkflows (wf:rest) counter =
>  do putStrLn $ "  " ++ (show counter) ++ ": " ++ wf
>     showWorkflows rest (counter + 1)

> getWorkflowList :: IO [String]
> getWorkflowList =
>     do conn <- DbUtil.openDbConnection
>        withTransaction conn (getWorkflowListFromDb)

> getWorkflowListFromDb :: (IConnection conn) => conn -> IO [String]
> getWorkflowListFromDb conn =
>     do rows <- quickQuery conn sql []
>        return $ map (fromSql.head) rows
>     where
>         sql = "select distinct name from wf_graph order by name asc"

> handleLoadError :: LoadException -> IO ()
> handleLoadError (LoadException msg) =
>     do putStrLn msg
>        return $ ()

> handleDbError :: SqlError -> IO ()
> handleDbError sqlError =
>     do putStrLn msg
>        return ()
>     where
>        msg = "Database error: " ++ (seErrorMsg sqlError)