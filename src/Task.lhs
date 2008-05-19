
> module Task where
> import Workflow

> data TaskState = Open | Complete | Rejected
>  deriving (Show,Eq)

> data Task =
>   Task {
>     getTokId       :: [Int],
>     taskId         :: String,
>     taskName       :: String,
>     taskDesc       :: String,
>     taskState      :: TaskState,
>     taskRejectable :: Bool
>   }

> showTaskList tasks =
>   do putStrLn "Tasks:"
>      if (null tasks)
>        then putStrLn "  No tasks to display"
>        else showTasks tasks 1

> showTasks [] _ = do return ()
> showTasks (task:rest) counter =
>  do putStrLn $ show counter ++ ": " ++ (taskName task) ++ " - " ++ show (taskState task)
>     showTasks rest (counter + 1)

> acceptAndCreateTask taskId name desc token wf@(WfInstance graph nodeTokens arcTokens tasks) =
>     return wf { userData = (newTask wf token taskId name desc):tasks }

> newTask wf token taskId name desc = Task (tokenId token) taskId name desc Open hasReject
>     where
>         nodeId = case token of (NodeToken _ nodeId) -> nodeId
>         hasReject     = not.null $ filter (\arc -> arcName arc =="reject") (outputs (wfGraph wf) nodeId)

> closeTask task wf newState = wf { userData = newTaskList }
>   where
>     newTaskList = map (closeIfMatches) (userData wf)
>     closeIfMatches t = if (taskId t == taskId task && (taskState t == Open))
>                            then t { taskState=newState }
>                            else t
>

> completeTask task wf = completeDefaultExecution token (closeTask task wf Complete)
>   where
>     token = getNodeTokenForId (getTokId task) wf

> rejectTask task wf = completeExecution token "reject" (closeTask task wf Rejected)
>   where
>     token = getNodeTokenForId (getTokId task) wf