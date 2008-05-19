> module Workflow.Util.ListUtil where
> import Data.Char

  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst _         [] = []
> removeFirst predicate (x:xs)
>     | predicate x = xs
>     | otherwise   = x : (removeFirst predicate xs)

> splitOn :: (Eq b) => (a->b) -> [a] -> [[a]]
> splitOn attr [] = []
> splitOn attr (x:xs) = firstSet : (splitOn attr rest)
>      where
>          firstSplit = extract (\x' -> attr x == attr x') xs
>          firstSet   = x:(fst firstSplit)
>          rest       = snd firstSplit

Given a predicate function and a list returns a pair of lists.
The first list contains all the elements that matched the predicate,
the second all other elements

> extract :: (a->Bool) -> [a] -> ([a],[a])
> extract f list = extract' f list [] []
>     where
>         extract' _ []     matches nonmatches = (matches, nonmatches)
>         extract' f (x:xs) matches nonmatches
>             | f x       = extract' f xs (x:matches) nonmatches
>             | otherwise = extract' f xs matches (x:nonmatches)

Removes leading whitespace

> trimLeading :: String -> String
> trimLeading [] = []
> trimLeading (x:xs) | isSpace x = trimLeading xs
>                    | otherwise = (x:xs)

Removes leading and trailing whitespace

> trim :: String -> String
> trim s = (trimLeading.reverse.trimLeading.reverse) s