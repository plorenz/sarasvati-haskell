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

module Workflow.Sarasvati.ListUtil where
import Data.Char

--  Removes the first instance in a list for which the given predicate
--  function returns true

removeFirst :: (a->Bool) -> [a] -> [a]
removeFirst _         [] = []
removeFirst predicate (x:xs)
    | predicate x = xs
    | otherwise   = x : (removeFirst predicate xs)

splitOn :: (Eq b) => (a->b) -> [a] -> [[a]]
splitOn _    []     = []
splitOn attr (x:xs) = firstSet : (splitOn attr rest)
     where
         firstSplit = extract (\x' -> attr x == attr x') xs
         firstSet   = x:(fst firstSplit)
         rest       = snd firstSplit

-- Given a predicate function and a list returns a pair of lists.
-- The first list contains all the elements that matched the predicate,
-- the second all other elements

extract :: (a->Bool) -> [a] -> ([a],[a])
extract f list = extract' f list [] []
    where
        extract' _ []     matches nonmatches = (matches, nonmatches)
        extract' f (x:xs) matches nonmatches
            | f x       = extract' f xs (x:matches) nonmatches
            | otherwise = extract' f xs matches (x:nonmatches)

-- Removes leading whitespace

trimLeading :: String -> String
trimLeading [] = []
trimLeading (x:xs) | isSpace x = trimLeading xs
                   | otherwise = (x:xs)

-- Removes leading and trailing whitespace

trim :: String -> String
trim s = (trimLeading.reverse.trimLeading.reverse) s

firstMatch :: (a-> Bool) -> [a] -> Maybe a
firstMatch _ []     = Nothing
firstMatch f (x:xs) | f x       = Just x
                    | otherwise = firstMatch f xs

removeNothings :: [Maybe a] -> [a]
removeNothings = (map (stripJust)).(filter (onlyJust))
    where
        onlyJust Nothing   = False
        onlyJust (Just _)  = True
        stripJust Nothing  = error "This should never happen: ListUtil.removeNothings"
        stripJust (Just x) = x