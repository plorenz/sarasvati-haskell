
> module TaskXml where
> import WorkflowXml
> import XmlUtil
> import Workflow
> import Task

> processTaskElement element source = Node 0 "task" nodeId source isJoinNode defaultGuard acceptFunction
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
>         acceptFunction = acceptAndCreateTask nodeId name desc
