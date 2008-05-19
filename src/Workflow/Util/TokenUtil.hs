module Workflow.Util.TokenUtil where

import Workflow.Engine

parentAttr :: ArcToken -> [TokenAttr]
parentAttr = tokenAttr.parentToken

mergeTokenAttrs :: [ArcToken] -> [TokenAttr]
mergeTokenAttrs [] = []
mergeTokenAttrs [token] = parentAttr token
mergeTokenAttrs arcList  = foldr1 (mergeAttrLists) (map (\t -> parentAttr t) arcList)

mergeAttrLists :: [TokenAttr] -> [TokenAttr] -> [TokenAttr]
mergeAttrLists list1 list2 = foldr (mergeAttr) list2 list1

mergeAttr :: TokenAttr -> [TokenAttr] -> [TokenAttr]
mergeAttr tokenAttr [] = [tokenAttr]
mergeAttr ins@(TokenAttr _ insKey _) (curr@(TokenAttr _ key _):xs)
    | insKey == key = curr : xs
    | otherwise     = curr : mergeAttr ins xs
