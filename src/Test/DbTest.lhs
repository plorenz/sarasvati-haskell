
> module Test.DbTest where
> import Database.HDBC.PostgreSQL
> import Database.HDBC
> import Workflow.Engine
> import qualified Data.Map as Map
> import qualified Control.Exception as Ex
> import Workflow.Util.ListUtil

> openConn = connectPostgreSQL "port=5433"

> listTables =
>     do conn <- openConn
>        tables <- getTables conn
>        mapM (\s-> putStrLn s) tables
>        disconnect conn

> persistGraph graph =
>    do catchSql (persistGraphInner graph)
>         (\e -> do putStrLn (seErrorMsg e)
>                   return 0)

> persistGraphInner (WfGraph _ name nodes _ outArcMap) =
>     do conn <- openConn
>        maxVersion <- getGraphVersionNumber conn name
>        graphId <- insertNewGraph conn name (maxVersion + 1)
>        persistNodes (Map.elems nodes)
>        commit conn
>        disconnect conn
>        return graphId

> getGraphVersionNumber conn name =
>     do rows <- quickQuery conn select [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         select = "select coalesce( max(version), 0) from wf_graph where name = ?"

> insertNewGraph :: (IConnection a) => a -> String -> Int -> IO Int
> insertNewGraph conn name version =
>     do stmt <- prepare conn sql
>        execute stmt [toSql name, toSql version]
>        finish stmt
>        getGraphId conn name version
>     where
>         sql = "insert into wf_graph (name, version) values ( ?, ? )"

> getGraphId conn name version =
>     do rows <- quickQuery conn select [toSql name, toSql version]
>        return $ (fromSql.head.head) rows
>     where
>         select = "select id from wf_graph where name = ? and version = ?"

> splitNodesBySource nodes = splitOn (nodeSource) nodes

> persistNodes nodes =
>     do print nodes
>        print (splitNodesBySource nodes)