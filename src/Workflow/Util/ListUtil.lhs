> module Workflow.Util.ListUtil where

  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>     | predicate x = xs
>     | otherwise   = x : (removeFirst predicate xs)

> splitOn attr [] = []
> splitOn attr (x:xs) = firstSet : (splitOn attr rest)
>      where
>          firstSplit = extract (\x' -> attr x == attr x') xs
>          firstSet   = x:(fst firstSplit)
>          rest       = snd firstSplit

> extract f list = extract' f list [] []
>     where
>         extract' _ []     matches nonmatches = (matches, nonmatches)
>         extract' f (x:xs) matches nonmatches
>             | f x       = extract' f xs (x:matches) nonmatches
>             | otherwise = extract' f xs matches (x:nonmatches)