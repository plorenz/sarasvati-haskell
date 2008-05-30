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


module Workflow.Task.TaskXml where

import Workflow.Util.XmlUtil
import Workflow.Util.ListUtil
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