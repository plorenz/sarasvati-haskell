
> module Workflow.Util.DbUtil where

> import Database.HDBC
> import Database.HDBC.PostgreSQL

> openDbConnection :: IO Connection
> openDbConnection = connectPostgreSQL "port=5433"

> nextSeqVal :: (IConnection a) => a -> String -> IO Int
> nextSeqVal conn name =
>     do rows <- quickQuery conn sql [toSql name]
>        return $ (fromSql.head.head) rows
>     where
>         sql = "select nextval( ? )"
