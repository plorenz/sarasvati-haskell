Author: Paul Lorenz

> module Workflow.UI.ConsoleDatabaseUI where

> import IO
> import Data.Char
> import Workflow.Loaders.DatabaseToEngineLoader
> import Workflow.Loaders.LoadError
> import qualified Data.Map as Map
> import qualified Workflow.Util.DbUtil as DbUtil
> import Database.HDBC
> import Database.HDBC.Types
> import Workflow.Engine
> import Workflow.DatabaseWfEngine
> -- import Workflow.MemoryWfEngine
> import Workflow.Task.Task
> import Workflow.Task.TaskDB
> import Workflow.UI.ConsoleCommon
> import Workflow.Util.DbUtil as DbUtil

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
>     do conn <- DbUtil.openDbConnection
>        let engine = DatabaseWfEngine conn
>        result <- startWorkflow engine nodeTypeMap graph []
>        case (result) of
>            Left msg -> do rollback conn
>                           putStrLn msg
>            Right wf -> do commit conn
>                           processTasks engine wf

> nodeTypeMap :: Map.Map String (NodeType [Task])
> nodeTypeMap = Map.fromList
>                 [ ( "start", NodeType defaultGuard completeDefaultExecution ),
>                   ( "node",  NodeType defaultGuard completeDefaultExecution ),
>                   ( "task",  NodeType defaultGuard acceptAndCreateTask ) ]

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