
> module Workflow.Loaders.XmlToDatabaseLoader where
> import Control.Exception
> import Control.Monad
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import qualified Data.Map as Map
> import Workflow.Util.XmlUtil as XmlUtil
> import Data.Dynamic

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
>         nodeId       :: Int,
>         nodeName     :: String,
>         arcs         :: [(String,Int)],
>         arcRefs      :: [(String,String)],
>         externalArcs :: [ExternalArc]
>     }
>  deriving (Show)

The ExternalArc contains all the information we need to load an external referenced workflow and
import it into the currently loading workflow.

> data ExternalArc =
>     ExternalArc {
>       targetNodeName :: String,
>       targetWf       :: String,
>       targetVersion  :: String,
>       targetInstance :: String,
>       extArcName     :: String,
>       arcType        :: ArcType
>     }
>  deriving (Show)

> data LoadArc =
>     LoadArc {
>         loadArcName  :: String,
>         loadArcRefA  :: Int,
>         loadArcRefZ  :: Int
>     }
>   deriving (Show)

> data LoadException = LoadException String
>   deriving (Show,Typeable)

> loadError msg = throwDyn $ LoadException msg

> handleLoad :: (LoadException -> IO a) -> IO a -> IO a
> handleLoad f a = catchDyn a f

================================================================================
= XML Functions
================================================================================

> loadDocFromFile :: FilePath -> IO (Either String Document)
> loadDocFromFile filename =
>     do fileContents <- readFile filename
>        return $ xmlParse' filename fileContents

> readArcs :: Element -> [(String, String)]
> readArcs element = map (readArc) arcChildren
>     where
>         attrVal (v,_) = v
>         arcChildren   = XmlUtil.toElem $ ((tag "arc") `o` children) (CElem element)
>         readArc e     = (readOptionalAttr e "name" "", readRequiredAttr e "to")

> readExternalArcs :: Element -> [ExternalArc]
> readExternalArcs element = map (readExternalArcFromElem) childElem
>     where
>         childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem e = ExternalArc nodeId workflowId version instanceId arcName arcType
>     where
>         workflowId = readRequiredAttr e "workflow"
>         version    = readRequiredAttr e "version"
>         instanceId = readRequiredAttr e "instance"
>         nodeId     = readRequiredAttr e "nodeId"
>         arcTypeS   = readRequiredAttr e "type"
>         arcName    = readOptionalAttr e "name" ""
>         arcType    = case (readRequiredAttr e "type") of
>                          "in"      -> InArc
>                          otherwise -> OutArc

> graphName doc = readRequiredAttr (rootElement doc) "id"

================================================================================
= Database Functions
================================================================================

> insertNewGraph :: (IConnection a) => a -> String -> IO Int
> insertNewGraph conn name =
>     do maxVersion <- getMaxGraphVersion conn name
>        putStrLn $ "Current version of " ++ name ++ " is " ++ (show maxVersion)
>        nextId <- nextSeqVal conn "wf_graph_id_seq"
>        run conn sql [toSql nextId,
>                      toSql name,
>                      toSql (maxVersion + 1)]
>        putStrLn $ "Inserted version " ++ show (maxVersion + 1) ++ " of " ++ name ++ " with id " ++ (show nextId)
>        return nextId
>     where
>         sql = "insert into wf_graph (id, name, version) values ( ?, ?, ? )"

> insertNodeWithRef :: (IConnection conn) => conn -> Int -> String -> Bool -> String -> IO (Int,Int)
> insertNodeWithRef conn graphId nodeName isJoin nodeType =
>     do nextNodeId <- nextSeqVal conn "wf_node_id_seq"
>        run conn nodeSql
>                  [toSql nextNodeId,
>                   toSql graphId,
>                   toSql nodeName,
>                   toSql isJoin,
>                   toSql nodeType]
>        nextNodeRefId <- nextSeqVal conn "wf_node_ref_id_seq"
>        run conn nodeRefSql
>                  [toSql nextNodeRefId,
>                   toSql nextNodeId,
>                   toSql graphId]
>        return (nextNodeId, nextNodeRefId)
>     where
>         nodeSql    = "insert into wf_node (id, graph_id, name, is_join, type) " ++
>                      " values ( ?, ?, ?, ?, ? )"
>         nodeRefSql = "insert into wf_node_ref (id, node_id, graph_id, instance) " ++
>                      " values (?, ?, ?, '' )"

> insertNodeRef :: (IConnection conn) => conn -> Int -> Int -> String -> IO Int
> insertNodeRef conn graphId copyRefId instanceName =
>     do putStrLn $ "Copying ref: " ++ (show copyRefId) ++ " using instance name: " ++ instanceName ++ " in graph: " ++ (show graphId)
>        nextNodeRefId <- nextSeqVal conn "wf_node_ref_id_seq"
>        run conn nodeRefSql
>                  [toSql nextNodeRefId,
>                   toSql graphId,
>                   toSql instanceName,
>                   toSql copyRefId]
>        return nextNodeRefId
>     where
>         nodeRefSql = "insert into wf_node_ref (id, node_id, graph_id, instance) " ++
>                      " select ?, node_id, ?, ? from wf_node_ref where id = ? "

> insertArc conn graphId startNode endNode arcName =
>     do nextArcId <- nextSeqVal conn "wf_arc_id_seq"
>        run conn sql
>                  [toSql nextArcId,
>                   toSql graphId,
>                   toSql startNode,
>                   toSql endNode,
>                   toSql arcName]
>     where
>         sql = "insert into wf_arc (id, graph_id, a_node_ref_id, z_node_ref_id, name ) " ++
>               " values ( ?, ?, ?, ?, ? ) "

> nextSeqVal :: (IConnection a) => a -> String -> IO Int
> nextSeqVal conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select nextval( ? )"

> getMaxGraphVersion :: (IConnection a) => a-> String -> IO Int
> getMaxGraphVersion conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select coalesce( max(version), 0) from wf_graph where name = ?"

> getMaxGraphId :: (IConnection a) => a-> String -> IO Int
> getMaxGraphId conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select coalesce( max(id), 0) from wf_graph where name = ?"

> getNodeRefId :: (IConnection a) => a-> Int -> String -> String -> String -> IO Int
> getNodeRefId conn newGraphId graphName nodeName instanceName =
>     do graphId <- getMaxGraphId conn graphName
>        rows <- quickQuery conn sql
>                  [toSql newGraphId,
>                   toSql instanceName,
>                   toSql nodeName,
>                   toSql graphId]
>        case (null rows) of
>            True -> loadError $ "Node with name " ++ nodeName ++ " for instance " ++ instanceName ++ " not found"
>            False -> return $ (fromSql.head.head) rows
>     where
>         sql = "select ref.id from wf_node_ref ref " ++
>               "               join wf_node node on (ref.node_id = node.id) " ++
>               "  where ref.graph_id = ? and ref.instance = ? " ++
>               "    and node.name = ? and node.graph_id = ? "

> getLoadArcs conn graphName =
>     do graphId <- getMaxGraphId conn graphName
>        rows <- quickQuery conn sql [toSql graphId]
>        return $ map (rowToLoadArc) rows
>     where
>         sql = "select name, a_node_ref_id, z_node_ref_id from wf_arc where graph_id = ?"

> rowToLoadArc row = LoadArc name refA refZ
>     where
>         name = fromSql $ row !! 0
>         refA = fromSql $ row !! 1
>         refZ = fromSql $ row !! 2

> openConn = connectPostgreSQL "port=5433"

================================================================================
= Load Functions
================================================================================

> processDoc doc funcMap conn =
>     do graphId   <- insertNewGraph conn (graphName doc)
>        loadNodes <- mapM (processChildNode graphId funcMap conn) childNodes
>        let resolvedLoadNodes = resolveArcs loadNodes
>        mapM (processArcs conn graphId) resolvedLoadNodes
>        processAllExternals conn graphId resolvedLoadNodes
>        return $ Right graphId
>     where
>         childNodes = getChildren (rootElement doc)

> processAllExternals conn graphId nodes =
>     foldM (processNodeExternals conn graphId) Map.empty nodes

> processNodeExternals conn graphId instanceMap node =
>     foldM (processExternal conn graphId node) instanceMap (externalArcs node)

> processExternal conn graphId node instanceMap extArc =
>     do putStrLn $ "Loading external: " ++ (show extArc)
>        newInstanceMap <- ensureInstanceLoaded conn instanceMap graphId extArc
>        targetNodeId <- getNodeRefId conn graphId wfName targetName instanceName
>        case (arcType extArc) of
>            InArc  -> insertArc conn graphId targetNodeId (nodeId node) (extArcName extArc)
>            OutArc -> insertArc conn graphId (nodeId node) targetNodeId (extArcName extArc)
>        return newInstanceMap
>     where
>        wfName       = targetWf extArc
>        instanceName = targetInstance extArc
>        targetName   = targetNodeName extArc

> ensureInstanceLoaded conn instanceMap graphId extArc =
>     case (Map.member instanceName instanceMap) of
>         True  -> return instanceMap
>         False -> importInstance conn instanceMap graphId extArc
>     where
>         instanceName = targetInstance extArc

> importInstance conn instanceMap graphId extArc =
>     do putStrLn $ "Importing: " ++ wfName
>        loadArcs <- getLoadArcs conn wfName
>        foldr (importArc conn graphId instanceName) startMap loadArcs
>        return $ Map.insert instanceName True instanceMap
>     where
>        wfName = targetWf extArc
>        instanceName = targetInstance extArc
>        startMap = return $ Map.empty

> importArc conn graphId instanceName (LoadArc arcName refA refZ) refMapIO =
>     do refMap  <- refMapIO
>        refMapA <- importNode conn graphId instanceName refA refMap
>        refMapZ <- importNode conn graphId instanceName refZ refMapA
>        insertArc conn graphId (refMapZ Map.! refA) (refMapZ Map.! refZ) arcName
>        return refMapZ

> importNode conn graphId instanceName refId refMap =
>     case (Map.member refId refMap) of
>         True  -> return refMap
>         False -> do newRefId <- insertNodeRef conn graphId refId instanceName
>                     return $ Map.insert refId newRefId refMap

================================================================================
= Load Functions - Arc related
================================================================================

> processArcs conn graphId loadNode = mapM (processArc) (arcs loadNode)
>     where
>         processArc arc = insertArc conn graphId startId (snd arc) (fst arc)
>         startId = nodeId loadNode

> resolveArcs loadNodes = map (resolveArcs') loadNodes
>     where
>         resolveArcs' loadNode = loadNode {arcs = map (resolveArc loadNodes) (arcRefs loadNode) }

> resolveArc loadNodes arc
>     | noTarget      = loadError $ "No node with name " ++ targetName ++
>                                   " found while looking for arc endpoint"
>     | toManyTargets = loadError $ "Too many nodes with name " ++ targetName ++
>                                   " found while looking for arc endpoint"
>     | otherwise     = (arcName, (nodeId.head) targetNodes)
>     where
>         arcName       = fst arc
>         targetName    = snd arc
>         targetNodes   = filter (\n->nodeName n == targetName) loadNodes
>         noTarget      = null targetNodes
>         toManyTargets = length targetNodes > 1

================================================================================
= Load Functions - Node related
================================================================================

> processChildNode graphId funcMap conn e =
>     do (nodeId, nodeName) <- nodeFunction e conn graphId
>        return $ LoadNode nodeId nodeName [] arcs extArcs
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = funcMap Map.! elemName
>         arcs         = readArcs e
>         extArcs      = readExternalArcs e

> processStart :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
> processStart element conn graphId =
>     do (nodeId, nodeRefId) <- insertNodeWithRef conn graphId "start" False "start"
>        return (nodeRefId, "start")

> processNode :: (IConnection conn) => Element -> conn -> Int -> IO (Int, String)
> processNode element conn graphId =
>    do (nodeId, nodeRefId) <- insertNodeWithRef conn graphId nodeName isJoin "node"
>       return (nodeRefId, nodeName)
>     where
>         nodeName = readRequiredAttr element "name"
>         isJoin = case (readOptionalAttr element "isJoin" "false" ) of
>                      "false"   -> False
>                      otherwise -> True

> loadFromXmlToDB ::
>      FilePath ->
>      Map.Map Name (Element -> Connection -> Int -> IO (Int,String)) ->
>      IO (Either String Int)
> loadFromXmlToDB filename funcMap =
>     do conn <- openConn
>        maybeDoc <- loadDocFromFile filename
>        case maybeDoc of
>            Left msg -> return $ Left msg
>            Right doc -> withTransaction conn (processDoc doc funcMap)

> initialLoaderMap = Map.fromList [ ("start", processStart),
>                                   ("node",  processNode) ]

> loaderMapWith list = addToMap list initialLoaderMap
>    where
>        addToMap []     map = map
>        addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map

> loadWorkflow filename funcMap = do handleAll (loadFromXmlToDB filename funcMap)
>     where
>         handleAll = (handleSql handleDbError).(handleLoad handleLoadError).(handleXml handleXmlError)

> testLoad filename = do handleAll (loadFromXmlToDB (prefix ++ filename) initialLoaderMap)
>     where
>         handleAll = (handleSql handleDbError).(handleLoad handleLoadError).(handleXml handleXmlError)
>         prefix    = "/home/paul/workspace/functional-workflow/"

> handleLoadError (LoadException msg) =
>     do putStrLn msg
>        return $ Left msg

> handleDbError sqlError =
>     do putStrLn msg
>        return $ Left msg
>     where
>        msg = "Database error: " ++ (seErrorMsg sqlError)

> handleXmlError (MissingRequiredAttr elemName attrName) =
>     do putStrLn msg
>        return $ Left msg
>     where
>         msg = "Missing xml attribute '" ++ attrName ++ "' on element of type '" ++ elemName ++ "'"