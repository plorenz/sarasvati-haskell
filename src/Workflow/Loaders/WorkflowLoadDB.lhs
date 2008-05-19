
> module Workflow.Loaders.WorkflowLoadDB where
> import Control.Exception
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import qualified Data.Map as Map
> import Workflow.Util.XmlUtil as XmlUtil

> loadDocFromFile :: FilePath -> IO (Either String Document)
> loadDocFromFile filename =
>     do fileContents <- readFile filename
>        return $ xmlParse' filename fileContents

> graphName doc = readRequiredAttr (rootElement doc) "id"

> insertNewGraph :: (IConnection a) => a -> String -> IO Int
> insertNewGraph conn name =
>     do maxVersion <- getMaxGraphId conn name
>        putStrLn $ "Current version of " ++ name ++ " is " ++ (show maxVersion)
>        nextId <- nextSeqVal conn "wf_graph_id_seq"
>        run conn sql [toSql nextId, toSql name, toSql (maxVersion + 1)]
>        putStrLn $ "Inserted version " ++ show (maxVersion + 1) ++ " of " ++ name ++ " with id " ++ (show nextId)
>        return nextId
>     where
>         sql = "insert into wf_graph (id, name, version) values ( ?, ?, ? )"

> nextSeqVal :: (IConnection a) => a -> String -> IO Int
> nextSeqVal conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select nextval( ? )"

> getMaxGraphId :: (IConnection a) => a-> String -> IO Int
> getMaxGraphId conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select coalesce( max(version), 0) from wf_graph where name = ?"

> openConn = connectPostgreSQL "port=5433"

> processDoc doc conn =
>     do graphId <- insertNewGraph conn (graphName doc)
>        return $ Right graphId

> loadFromXmlToDB :: FilePath -> IO (Either String Int)
> loadFromXmlToDB filename =
>     do conn <- openConn
>        maybeDoc <- loadDocFromFile filename
>        case maybeDoc of
>            Left msg -> return $ Left msg
>            Right doc -> withTransaction conn (processDoc doc)

> testLoad2 filename = do handleAll (loadFromXmlToDB filename)
>     where
>         handleAll = (handleSql handleDbError).(handleXml handleXmlError)

> handleDbError sqlError = do putStrLn $ "Database error: " ++ (seErrorMsg sqlError)
>                             return $ Left $ "Database error: " ++ (seErrorMsg sqlError)

> handleXmlError (MissingRequiredAttr elemName attrName) =
>     do putStrLn $ "Missing xml attribute " ++ attrName ++ " on element " ++ elemName
>        return $ Left $ "Missing xml attribute " ++ attrName ++ " on element " ++ elemName