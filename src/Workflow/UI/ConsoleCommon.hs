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

module Workflow.UI.ConsoleCommon where

import Workflow.Engine
import Workflow.Task.Task
import Data.Char
import Random

handleTask :: (WfEngine e) => e -> Task -> WfProcess [Task] -> IO (WfProcess [Task])
handleTask engine task wf =
    do putStrLn $ "Task name: " ++ (taskName task)
       putStrLn $ "Task desc: " ++ (taskDesc task)
       putStrLn $ "Task state: " ++ show (taskState task)
       case (taskState task) of
           Open -> do putStr prompt
                      response <- getLine
                      case (response) of
                          "1" -> do newWf <- completeTask engine task wf
                                    transactionBoundary engine
                                    putStrLn "Task Completed"
                                    return newWf
                          "2" -> if (rejectable)
                                     then do newWf <- rejectTask engine task wf
                                             transactionBoundary engine
                                             putStrLn "Task Rejected"
                                             return newWf
                                     else do putStrLn "Ok. Leaving open"
                                             return wf
                          _   -> do putStrLn "Ok. Leaving open"
                                    return wf
           Complete -> return wf
           Rejected -> return wf
    where
        rejectable = taskRejectable task
        prompt = case (rejectable) of
                     True ->  "1. Complete task\n2. Reject task\n3. Leave task open\nAction: "
                     False -> "1. Complete task\n2. Leave task open\nAction: "

getTask :: Integer -> [Task] -> Either String Task
getTask _ [] = Left "Invalid task number"
getTask taskNumber (first:rest)
    | taskNumber <  1 = Left "Invalid task number"
    | taskNumber == 1 = Right first
    | otherwise       = getTask (taskNumber - 1) rest

showTokens :: (Show a) => [a] -> IO ()
showTokens []     = return ()
showTokens (x:xs) = do putStrLn (show x)
                       showTokens xs

processTasks :: (WfEngine e) => e -> WfProcess [Task] -> IO ()
processTasks _         (WfProcess _ _ _ [] [] _ _ _    ) = putStrLn "Workflow complete!"
processTasks engine wf@(WfProcess _ _ _ _  _  _ _ tasks) =
    do putStrLn ""
       showTaskList tasks
       putStr "> "
       cmd <- getLine
       case (getCmdType cmd) of
           ShowTokenCmd -> do putStrLn "Node Tokens"
                              showTokens (nodeTokens wf)
                              putStrLn "\nArc Tokens"
                              showTokens (arcTokens wf)
                              processTasks engine wf
           TaskCmd ->
               case (getTask ((read cmd)::Integer) tasks) of
                   Left msg -> do putStrLn msg
                                  processTasks engine wf
                   Right task -> do newWf <- handleTask engine task wf
                                    processTasks engine newWf
           BadCmd -> do putStrLn $ cmd ++ " is not a valid command or task entry"
                        processTasks engine wf
           NoCmd  -> processTasks engine wf

data CmdType = ShowTokenCmd | TaskCmd | BadCmd | NoCmd

getCmdType :: String -> CmdType
getCmdType input
    | null input                   = NoCmd
    | (map (toUpper) input) == "T" = ShowTokenCmd
    | all (isDigit) input          = TaskCmd
    | otherwise                    = BadCmd

showWorkflows :: [String] -> Int -> IO ()
showWorkflows []        _       = return ()
showWorkflows (wf:rest) counter =
    do putStrLn $ "  " ++ (show counter) ++ ": " ++ wf
       showWorkflows rest (counter + 1)

acceptInit :: (WfEngine engine) => engine -> NodeToken -> WfProcess a -> IO (WfProcess a)
acceptInit engine token process =
    do process <- setTokenAttr engine process token "iter" (show newVal)
       nextRand <- getStdRandom (randomR (1,2))::(IO Int)
       putStrLn $ "Next random: " ++ (show nextRand)
       process <- setTokenAttr engine process token "rand" (show nextRand)
       completeDefaultExecution engine token process
    where
        newVal = case (attrValue process token "iter") of
                     Nothing -> 0
                     Just x  -> (read x::Int) + 1

acceptDump :: (WfEngine engine) => engine -> NodeToken -> WfProcess a -> IO (WfProcess a)
acceptDump engine token process =
    do putStrLn $ "Accepted into " ++ (nodeName node)
       completeDefaultExecution engine token process
    where
       node = nodeForToken token (wfGraph process)

predIsRandOdd :: NodeToken -> WfProcess a -> IO Bool
predIsRandOdd token process = return isOdd
    where
       randVal = attrValueReq process token "rand"
       isOdd   = (read randVal::Int) `mod` 2 == 0

predIsRandEven :: NodeToken -> WfProcess a -> IO Bool
predIsRandEven token process =
    do result <- predIsRandOdd token process
       return $ not result

predIsTenthIteration :: NodeToken -> WfProcess a -> IO Bool
predIsTenthIteration token process = return isTenth
    where
       iterVal = attrValueReq process token "iter"
       isTenth = (read iterVal::Int) >= 10