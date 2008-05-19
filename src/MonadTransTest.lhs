
> module MonadTransTest where
> import Control.Monad.State.Lazy
> import Control.Monad.Error
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types

> parseNode e = do nodeId <- readAttr e "nodeId"
>                  nodeType <- readAttr e "type"
>                  liftIO $ putStrLn "hello"
>                  return (id, nodeType)

> readAttr (Elem elemName attrList _ ) name
>      | null attrs = throwError $ "Attribute " ++ name ++ " not found in element " ++ elemName
>      | otherwise  = return $ attrVal' (head attrs)
>     where
>         attrs = filter (\(attrName, attrValue) -> attrName == name) attrList
>         attrVal' (_, AttValue atlist) = case (head atlist) of (Left val) -> val
