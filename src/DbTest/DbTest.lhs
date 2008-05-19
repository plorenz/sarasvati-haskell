
> module DbTest.DbTest where
> import Database.HDBC.PostgreSQL
> import Database.HDBC
> import Workflow

> openConn = connectPostgreSQL ""

> listTables =
>     do conn <- openConn
>        tables <- getTables conn
>        mapM (\s-> putStrLn s) tables
>        disconnect conn

 persistGraph (WfGraph nodes _ outArcMap)