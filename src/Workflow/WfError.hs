-- Author: Paul

module Workflow.WfError where
import Data.Dynamic
import Control.Exception

data WfException = WfException String
  deriving (Show,Typeable)

wfError :: String -> a
wfError msg = throwDyn $ WfException msg

handleWfError :: (WfException -> IO a) -> IO a -> IO a
handleWfError f a = catchDyn a f
