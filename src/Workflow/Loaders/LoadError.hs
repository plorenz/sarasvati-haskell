-- Author: Paul

module Workflow.Loaders.LoadError where
import Data.Dynamic
import Control.Exception

data LoadException = LoadException String
  deriving (Show,Typeable)

loadError msg = throwDyn $ LoadException msg

handleLoad :: (LoadException -> IO a) -> IO a -> IO a
handleLoad f a = catchDyn a f
