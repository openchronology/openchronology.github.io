module Main where

import Components.Index (index)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

import ReactDOM (render)
import React (ReactElement, ReactComponent)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)


mountToRoot :: ReactElement -> Effect (Maybe ReactComponent)
mountToRoot x = do
  doc <- (toNonElementParentNode <<< toDocument) <$> (window >>= document)
  mEl <- getElementById "root" doc
  case mEl of
    Nothing -> throw "No #root <div> node!"
    Just el -> render x el


main :: Effect Unit
main = do
  log "Booting up application"

  void (mountToRoot index)
