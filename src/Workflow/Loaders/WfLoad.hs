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

module Workflow.Loaders.WfLoad where

-- | Enumerates the kind of external arcs allowed, which are just outgoing arcs and incoming arcs.
--   Internal arcs are all defined as outgoing, but because external arcs must add arcs to
--   nodes not in the same workflow, they are allowed both.

data ArcType = InArc | OutArc
  deriving (Show)

-- | Contains all the information we need to load an external referenced workflow and
--   import it into the currently loading workflow.

data ExternalArc =
    ExternalArc {
      targetNodeName :: String,
      targetWf       :: String,
      targetInstance :: String,
      extArcName     :: String,
      arcType        :: ArcType
    }
 deriving (Show)
