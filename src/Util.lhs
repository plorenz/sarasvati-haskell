> module Util where

  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>     | predicate x = xs
>     | otherwise = x : (removeFirst predicate xs)
