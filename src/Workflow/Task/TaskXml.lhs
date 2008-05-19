
> module Workflow.Task.TaskXml where
> import Workflow.Loaders.WorkflowLoadXml
> import Workflow.Util.XmlUtil
> import Workflow.Workflow
> import Workflow.Task.Task
> import Data.Dynamic

> processTaskElement element source = Node 0 "task" nodeId source isJoinNode nodeExtra
>     where
>         nodeId         = readAttr element "nodeId"
>         nodeTypeStr    = readAttr element "type"
>         name           = readText element "name"
>         desc           = readText element "description"
>
>         isJoinNode     = case (nodeTypeStr) of
>                              "requireSingle" -> False
>                              otherwise       -> True
>
>         taskDef        = TaskDef name desc
>         nodeExtra      = mkNodeExtra taskDef