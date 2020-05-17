{-
OpenChronology - an application for graphing and visualizing timelines.
Copyright (C) 2020  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module Main where

{-|

This module just gets everything started and booted up. There are a few things to consider
from this standpoint:

- A higher-order container is wrapped around `Index` with `mountToRoot`
- `main` kicks off the application

-}
import Components.Index (index)
import Plumbing (newPrimaryQueues, newPrimarySignals, logic)
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref (new) as Ref
import ReactDOM (render)
import React (ReactElement, ReactComponent)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)

-- | Get the `Document` node, and look for the `<div id='root'></div>` element
-- | (found in the templates - see [build/README.md](../../build/README.md)).
mountToRoot :: ReactElement -> Effect (Maybe ReactComponent)
mountToRoot x = do
  doc <- (toNonElementParentNode <<< toDocument) <$> (window >>= document)
  mEl <- getElementById "root" doc
  case mEl of
    Nothing -> throw "No #root <div> node!"
    Just el -> render x el -- hand-off to React.js

-- | Start application
main :: Effect Unit
main = do
  log "Booting up application"
  stateRef <- Ref.new unit
  -- initialize asynchronous signals and queues
  primaryQueues <- newPrimaryQueues
  -- shared state signals
  primarySignals <- newPrimarySignals
  -- create logic functions
  let
    logicFunctions = logic primaryQueues primarySignals
  -- `mountToRoot` returns the react component generated when binding to the DOM node #root
  void $ mountToRoot
    $ index
        { stateRef
        , primaryQueues
        , primarySignals
        , logicFunctions
        }
