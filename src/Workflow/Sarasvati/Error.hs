{-
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
-}

module Workflow.Sarasvati.Error where

import Data.Dynamic
import Control.Exception

data WfError = WfError String
  deriving (Show,Typeable)

wfError :: String -> a
wfError msg = throwDyn $ WfError msg

handleWf :: (WfError -> IO a) -> IO a -> IO a
handleWf f a = catchDyn a f

wfErrorToLeft :: WfError -> IO (Either String a)
wfErrorToLeft (WfError msg) = return $ Left msg

catchWf :: IO (Either String a) -> IO (Either String a)
catchWf = handleWf wfErrorToLeft