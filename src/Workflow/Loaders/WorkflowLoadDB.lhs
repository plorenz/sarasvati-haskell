
> module Workflow.Loaders.WorkflowLoadDB where
> import Control.Exception
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import qualified Data.Map as Map
> import Workflow.Util.XmlUtil as XmlUtil

================================================================================
= Data Type Definitions
================================================================================


ArcType enumerates the kind of external allows, which are just outgoing arcs and incoming arcs.
General arcs are all defined as outgoing, but because external arcs must add outgoing arcs to
nodes not in the same workflow, they are allowed both.

> data ArcType = InArc | OutArc
>   deriving (Show)

The NodeArcs gives both incoming and outgoing nodes. However, when loading a graph, the arcs are
defined only one way (for simplicity and correctness). The LoadNode maps to what a workflow graph
definition node mostly likely looks like.

> data LoadNode =
>     LoadNode {
>         nodeRefId    :: Int,
>         arcs         :: [(String,Int)],
>         arcRefs      :: [(String,String)],
>         externalArcs :: [ExternalArc]
>     }
>  deriving (Show)

The ExternalArc contains all the information we need to load an external referenced workflow and
import it into the currently loading workflow.

> data ExternalArc =
>     ExternalArc {
>       targetNodeRef  :: String,
>       targetWf       :: String,
>       targetVersion  :: String,
>       targetInstance :: String,
>       extArcName     :: String,
>       arcType        :: ArcType
>     }
>  deriving (Show)

================================================================================
= XML Functions
================================================================================

> loadDocFromFile :: FilePath -> IO (Either String Document)
> loadDocFromFile filename =
>     do fileContents <- readFile filename
>        return $ xmlParse' filename fileContents

> readArcs element = map (toArc) arcChildren
>     where
>         attrVal (v,_) = v
>         arcChildren   = XmlUtil.toElem $ ((tag "arc") `o` children) (CElem element)
>         toArc e       = (readOptionalAttr e "name" "", readRequiredAttr e "to")
>

> readExternalArcs element = map (readExternalArcFromElem) childElem
>     where
>         childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem e = ExternalArc nodeId workflowId version instanceId arcName arcType
>     where
>        workflowId = readRequiredAttr e "workflow"
>        version    = readRequiredAttr e "version"
>        instanceId = readRequiredAttr e "instance"
>        nodeId     = readRequiredAttr e "nodeId"
>        arcName    = readOptionalAttr e "name" ""
>        arcType    = case (readRequiredAttr e "type") of
>                         "in"      -> InArc
>                         otherwise -> OutArc

> graphName doc = readRequiredAttr (rootElement doc) "id"

================================================================================
= Database Functions
================================================================================

> insertNewGraph :: (IConnection a) => a -> String -> IO Int
> insertNewGraph conn name =
>     do maxVersion <- getMaxGraphId conn name
>        putStrLn $ "Current version of " ++ name ++ " is " ++ (show maxVersion)
>        nextId <- nextSeqVal conn "wf_graph_id_seq"
>        run conn sql [toSql nextId,
>                      toSql name,
>                      toSql (maxVersion + 1)]
>        putStrLn $ "Inserted version " ++ show (maxVersion + 1) ++ " of " ++ name ++ " with id " ++ (show nextId)
>        return nextId
>     where
>         sql = "insert into wf_graph (id, name, version) values ( ?, ?, ? )"

> insertNodeWithRef conn graphId nodeName isJoin nodeType =
>      do nextNodeId <- nextSeqVal conn "wf_node_id_seq"
>         run conn nodeSql
>                   [toSql nextNodeId,
>                    toSql graphId,
>                    toSql nodeName,
>                    toSql isJoin,
>                    toSql nodeType]
>         nextNodeRefId <- nextSeqVal conn "wf_node_ref_id_seq"
>         run conn nodeRefSql
>                   [toSql nextNodeRefId,
>                    toSql nextNodeId,
>                    toSql (1::Int)]
>         return nextNodeRefId
>      where
>          nodeSql = "insert into wf_node (id, graph_id, name, is_join, type) " ++
>                    " values ( ?, ?, ?, ?, ? )"
>          nodeRefSql = "insert into wf_node_ref (id, node_id, instance) " ++
>                       " values (?, ?, ? )"

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

================================================================================
= Load Functions
================================================================================

> processDoc doc funcMap conn =
>     do graphId <- insertNewGraph conn (graphName doc)
>        loadNodes <- mapM (processChildNode graphId funcMap conn) childNodes
>        return $ Right graphId
>     where
>         childNodes = getChildren (rootElement doc)

> processChildNode graphId funcMap conn e =
>     do nodeId <- nodeFunction e conn graphId
>        return $ LoadNode nodeId [] (readArcs e) (readExternalArcs e)
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = funcMap Map.! elemName

> processStart :: (IConnection conn) => Element -> conn -> Int -> IO Int
> processStart element conn graphId =
>     insertNodeWithRef conn graphId "start" False "start"

> processNode :: (IConnection conn) => Element -> conn -> Int -> IO Int
> processNode element conn graphId = insertNodeWithRef conn graphId name isJoin "node"
>     where
>         name = readRequiredAttr element "name"
>         isJoin = case (readOptionalAttr element "isJoin" "false" ) of
>                      "false"   -> False
>                      otherwise -> True

> loadFromXmlToDB ::
>      FilePath ->
>      Map.Map Name (Element -> Connection -> Int -> IO Int) ->
>      IO (Either String Int)
> loadFromXmlToDB filename funcMap =
>     do conn <- openConn
>        maybeDoc <- loadDocFromFile filename
>        case maybeDoc of
>            Left msg -> return $ Left msg
>            Right doc -> withTransaction conn (processDoc doc funcMap)

> funcMap = Map.fromList [ ("start", processStart),
>                          ( "node", processNode) ]

> testLoad filename = do handleAll (loadFromXmlToDB (prefix ++ filename) funcMap)
>     where
>         handleAll = (handleSql handleDbError).(handleXml handleXmlError)
>         prefix    = "/home/paul/workspace/functional-workflow/"

> handleDbError sqlError = do putStrLn $ "Database error: " ++ (seErrorMsg sqlError)
>                             return $ Left $ "Database error: " ++ (seErrorMsg sqlError)

> handleXmlError (MissingRequiredAttr elemName attrName) =
>     do putStrLn $ "Missing xml attribute " ++ attrName ++ " on element " ++ elemName
>        return $ Left $ "Missing xml attribute " ++ attrName ++ " on element " ++ elemName