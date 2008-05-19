
> module Workflow.Util.DbUtil where
> import Database.HDBC
> import Database.HDBC.PostgreSQL

> openDbConnection :: IO Connection
> openDbConnection = connectPostgreSQL "port=5433"