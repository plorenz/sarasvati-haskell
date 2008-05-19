
> module Test.ExceptionTest where
> import Data.Dynamic
> import Control.Exception

> data XmlException =
>     MissingRequiredAttr {
>        elementName :: String,
>        attrName    :: String
>     }
>   deriving (Typeable)

> missingAttr elemName attrName = throwDyn $ MissingRequiredAttr elemName attrName

> readAttr e attr =
>     do missingAttr e attr

> readNode = do attr1 <- readAttr "a" "b"
>               attr2 <- readAttr "c" "d"
>               putStrLn attr1
>               return $ attr1 ++ attr2

> handleXmlError (MissingRequiredAttr name attr) = Left $ "Element " ++ name ++ " missing required attribute " ++ attr