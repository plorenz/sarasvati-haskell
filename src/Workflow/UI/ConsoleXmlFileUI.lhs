Author: Paul Lorenz

> module Workflow.UI.ConsoleXmlFileUI where

> import Workflow.EngineTypes
> import Workflow.Engine
> import Workflow.Task.Task
> import IO
> import Data.Char
> import System.Directory
> import Workflow.Loaders.WorkflowLoadXml
> import Workflow.Task.TaskXml
> import qualified Data.Map as Map
> import Workflow.UI.ConsoleCommon
> import Workflow.MemoryWfEngine

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
>          then useWorkflow wfList (((read wf)::Int) - 1)
>          else do putStrLn $ "ERROR: " ++ wf ++ " is not a valid workflow"
>        selectWorkflow wfList

> useWorkflow :: [String] -> Int -> IO ()
> useWorkflow wfList idx
>     | length wfList <= idx = do putStrLn "ERROR: Invalid workflow number"
>     | otherwise            = do result <- loadWfGraphFromFile (wfList !! idx) elemFunctionMap
>                                 case (result) of
>                                     Left msg -> putStrLn $ "ERROR: Could not load workflow: " ++ msg
>                                     Right graph -> do putStrLn "Running workflow"
>                                                       putStrLn (showGraph graph)
>                                                       runWorkflow graph
>    where
>        elemFunctionMap = elemMapWith [ ("task", processTaskElement) ]

> runWorkflow :: WfGraph -> IO ()
> runWorkflow graph =
>     do engine <- newMemoryWfEngine
>        result <- startWorkflow engine nodeTypeMap graph []
>        case (result) of
>            Left msg -> putStrLn msg
>            Right wf -> processTasks engine wf

> nodeTypeMap :: Map.Map String (NodeType [Task])
> nodeTypeMap = Map.fromList
>                 [ ( "start", NodeType defaultGuard completeDefaultExecution ),
>                   ( "node",  NodeType defaultGuard completeDefaultExecution ),
>                   ( "task",  NodeType defaultGuard acceptAndCreateTask ) ]

> getWorkflowList :: IO [String]
> getWorkflowList =
>     do fileList <- getDirectoryContents wfDir
>        return $ (useFullPath.filterWfs) fileList
>   where
>     wfDir = "/home/paul/workspace/functional-workflow/test-wf/"
>     filterWfs = (filter (hasExtension ".wf.xml"))
>     useFullPath = (map (\f->wfDir ++ f))

> hasExtension :: String -> String -> Bool
> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)