
> module Workflow.Loaders.WorkflowLoadDB where
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Workflow.Util.XmlUtil as XmlUtil

> loadDocFromFile filename =
>     do fileContents <- readFile filename
>        return $ xmlParse' filename fileContents

> loadFromXmlToDB filename funcMap = loadDocFromFile filename