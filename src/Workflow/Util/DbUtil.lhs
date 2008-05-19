
> module Workflow.Util.DbUtil where

> import Database.HDBC.PostgreSQL

> openDbConnection :: IO Connection
> openDbConnection = connectPostgreSQL "port=5433"