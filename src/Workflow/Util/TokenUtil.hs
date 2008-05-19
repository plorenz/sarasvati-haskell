module Workflow.Util.TokenUtil where

import Workflow.EngineTypes
import Workflow.Engine

parentAttrs :: WfProcess a -> ArcToken -> [TokenAttr]
parentAttrs wfProcess = (tokenAttrs wfProcess).parentToken

mergeTokenAttrs :: WfProcess a -> [ArcToken] -> [TokenAttr]
mergeTokenAttrs _ []            = []
mergeTokenAttrs process [token] = parentAttrs process token
mergeTokenAttrs process arcList = foldr1 (mergeAttrLists) (map (\t -> parentAttrs process t) arcList)

mergeAttrLists :: [TokenAttr] -> [TokenAttr] -> [TokenAttr]
mergeAttrLists list1 list2 = foldr (mergeAttr) list2 list1

mergeAttr :: TokenAttr -> [TokenAttr] -> [TokenAttr]
mergeAttr tokenAttr [] = [tokenAttr]
mergeAttr ins@(TokenAttr _ insKey _) (curr@(TokenAttr _ key _):xs)
    | insKey == key = curr : xs
    | otherwise     = curr : mergeAttr ins xs

setTokenAttr :: WfProcess a -> NodeToken -> TokenAttr -> WfProcess a
setTokenAttr process token attr = replaceTokenAttrs process token newAttrList
    where
       newAttrList = setOrReplaceTokenAttr (tokenAttrs process token) attr

setOrReplaceTokenAttr :: [TokenAttr] -> TokenAttr -> [TokenAttr]
setOrReplaceTokenAttr [] attr = [attr]
setOrReplaceTokenAttr (curr@(TokenAttr _ key _):xs) new@(TokenAttr _ newKey _)
   | key == newKey = new : xs
   | otherwise     = curr : setOrReplaceTokenAttr xs new

removeTokenAttr :: WfProcess a -> NodeToken -> String -> WfProcess a
removeTokenAttr process token key = replaceTokenAttrs process token newAttrList
    where
       newAttrList = removeTokenAttrFromList key (tokenAttrs process token)

removeTokenAttrFromList ::  String -> [TokenAttr] -> [TokenAttr]
removeTokenAttrFromList key = filter (\(TokenAttr _ name _) -> key /= name)

nodeHasAttr :: (WfProcess a) -> NodeToken -> String -> Bool
nodeHasAttr wfProcess nodeToken key = any (\(TokenAttr _ name _) -> key == name) (tokenAttrs wfProcess nodeToken)