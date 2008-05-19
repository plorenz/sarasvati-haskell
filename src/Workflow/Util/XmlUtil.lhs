
> module Workflow.Util.XmlUtil where
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Control.Monad.Error
> import Data.Dynamic
> import Control.Exception

> data XmlException = MissingRequiredAttr String String
>   deriving (Show,Typeable)

> readAttr element name = head $ map (\(val,_)->val) $ attributed name (keep) (CElem element)

> missingAttr elemName attrName = throwDyn $ MissingRequiredAttr elemName attrName

> handleXml :: (XmlException -> IO a) -> IO a -> IO a
> handleXml f a = catchDyn a f

> readRequiredAttr element name
>     | null attrList = missingAttr (elementName element) name
>     | otherwise     = head attrList
>     where
>         attrList = map (\(val,_)->val) $ attributed name (keep) (CElem element)

> readOptionalAttr element name defaultValue
>     | null attrList = defaultValue
>     | otherwise     = head attrList
>     where
>         attrList = map (\(val,_)->val) $ attributed name (keep) (CElem element)

> cElemToElem (CElem element) = element

> toElem = map (cElemToElem)

> elementName (Elem name _ _) = name

> getChildren element = toElem $ (elm `o` children) (CElem element)

> rootElement (Document _ _ element _ ) = element

> readText element name = concatMap (stripMaybe.fst) $ filter (onlyJust) labels
>     where
>         onlyJust (Nothing,_)  = False
>         onlyJust ((Just _),_) = True
>         stripMaybe (Just x)   = x
>         labels                = textlabelled ( path [ children, tag name, children, txt ] ) (CElem element)
