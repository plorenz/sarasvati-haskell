
module Workflow.Task.TaskXml where

import Workflow.Util.XmlUtil
import Workflow.Util.ListUtil
import Workflow.EngineTypes
import Workflow.Engine
import Workflow.Task.Task
import Text.XML.HaXml.Types

processTaskElement :: Element -> NodeSource -> Node
processTaskElement element source = Node 0 "task" nodeId source isJoinNode guard nodeExtra
    where
        nodeId         = readAttr element "nodeId"
        nodeTypeStr    = readAttr element "type"
        name           = readText element "name"
        desc           = readText element "description"
        guard          = trim $ readText element "guard"

        isJoinNode     = case (nodeTypeStr) of
                             "requireSingle" -> False
                             _               -> True

        taskDef        = TaskDef name desc
        nodeExtra      = makeNodeExtra taskDef