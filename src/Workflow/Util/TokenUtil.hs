module Workflow.Util.TokenUtil where

import Workflow.Engine

parentAttrs :: ArcToken -> [TokenAttr]
parentAttrs = tokenAttrs.parentToken

mergeTokenAttrs :: [ArcToken] -> [TokenAttr]
mergeTokenAttrs [] = []
mergeTokenAttrs [token] = parentAttrs token
mergeTokenAttrs arcList  = foldr1 (mergeAttrLists) (map (\t -> parentAttrs t) arcList)

mergeAttrLists :: [TokenAttr] -> [TokenAttr] -> [TokenAttr]
mergeAttrLists list1 list2 = foldr (mergeAttr) list2 list1

mergeAttr :: TokenAttr -> [TokenAttr] -> [TokenAttr]
mergeAttr tokenAttr [] = [tokenAttr]
mergeAttr ins@(TokenAttr _ insKey _) (curr@(TokenAttr _ key _):xs)
    | insKey == key = curr : xs
    | otherwise     = curr : mergeAttr ins xs

setOrReplace :: [TokenAttr] -> TokenAttr -> [TokenAttr]
setOrReplace [] attr = [attr]
setOrReplace (curr@(TokenAttr _ key oldValue):xs) new@(TokenAttr _ newKey newValue)
   | key == newKey = new : xs
   | otherwise     = curr : setOrReplace xs new