Author: Paul Lorenz

> module Main where
> import Workflow
> import Task
> import IO
> import Data.Char
> import System.Directory
> import WorkflowXml
> import TaskXml
> import qualified Data.Map as Map
> import DbTest.DbTest

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
>                           otherwise -> do putStrLn "Ok. Leaving open"
>                                           return wf
>            Complete -> return wf
>     where
>         rejectable = taskRejectable task
>         prompt = case (rejectable) of
>                      True ->  "1. Complete task\n2. Reject task\n3. Leave task open\nAction: "
>                      False -> "1. Complete task\n2. Leave task open\nAction: "

> getTask _ [] = Left "Invalid task number"
> getTask taskNumber tasks@(first:rest)
>     | taskNumber <  1 = Left "Invalid task number"
>     | taskNumber == 1 = Right first
>     | otherwise       = getTask (taskNumber - 1) rest

> showTokens []     = return ()
> showTokens (x:xs) = do putStrLn (show x)
>                        showTokens xs

> processTasks wf@(WfInstance _         _     []         []        _    ) = putStrLn "Workflow complete!"
> processTasks wf@(WfInstance nodeTypes graph nodeTokens arcTokens tasks) =
>     do putStrLn ""
>        showTaskList tasks
>        putStr "> "
>        cmd <- getLine
>        case (getCmdType cmd) of
>            ShowTokenCmd -> do putStrLn "Node Tokens"
>                               showTokens nodeTokens
>                               putStrLn "\nArc Tokens"
>                               showTokens arcTokens
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
>            PersistCmd -> do persistGraph graph
>                             processTasks wf

> data CmdType = ShowTokenCmd | TaskCmd | BadCmd | NoCmd | PersistCmd

> getCmdType input
>     | null input                   = NoCmd
>     | (map (toUpper) input) == "T" = ShowTokenCmd
>     | (map (toUpper) input) == "P" = PersistCmd
>     | all (isDigit) input          = TaskCmd
>     | otherwise                    = BadCmd

> main =
>     do hSetBuffering stdout NoBuffering
>        wfList <- getWorkflowList
>        selectWorkflow wfList

> selectWorkflow wfList =
>     do putStrLn "\n-=Available workflows=-"
>        showWorkflows wfList 1
>        putStr "\nSelect workflow: "
>        wf <- getLine
>        if ((not $ null wf) && all (isDigit) wf)
>          then useWorkflow wfList (((read wf)::Int) - 1)
>          else do putStrLn $ "ERROR: " ++ wf ++ " is not a valid workflow"
>        selectWorkflow wfList

> useWorkflow wfList idx
>     | length wfList <= idx = do putStrLn "ERROR: Invalid workflow number"
>     | otherwise            = do result <- loadWfGraphFromFile (wfList !! idx) elemFunctionMap
>                                 case (result) of
>                                     Left msg -> putStrLn $ "ERROR: Could not load workflow: " ++ msg
>                                     Right wfGraph -> do putStrLn "Running workflow"
>                                                         putStrLn (showGraph wfGraph)
>                                                         runWorkflow wfGraph
>    where
>        elemFunctionMap = elemMapWith [ ("task", processTaskElement) ]

> runWorkflow wfGraph =
>     case (startWorkflow nodeTypeMap wfGraph []) of
>            Left msg -> putStrLn msg
>            Right wfInstanceIO -> do wf <- wfInstanceIO
>                                     processTasks wf

> nodeTypeMap = Map.fromList
>                 [ ( "start", NodeType defaultGuard completeDefaultExecution ),
>                   ( "node",  NodeType defaultGuard completeDefaultExecution ),
>                   ( "task",  NodeType defaultGuard acceptAndCreateTask ) ]

> showWorkflows []        _       = return ()
> showWorkflows (wf:rest) counter =
>  do putStrLn $ "  " ++ (show counter) ++ ": " ++ wf
>     showWorkflows rest (counter + 1)

> getWorkflowList =
>     do fileList <- getDirectoryContents wfDir
>        return $ (useFullPath.filterWfs) fileList
>   where
>     wfDir = "/home/paul/workspace/functional-workflow/test-wf/"
>     filterWfs = (filter (hasExtension ".wf.xml"))
>     useFullPath = (map (\f->wfDir ++ f))

> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)