module Test.Test where

test = case 1 of
           1    -> putStrLn "hello"
           2    -> putStrLn "bye"
           _    -> putStrLn "foo"