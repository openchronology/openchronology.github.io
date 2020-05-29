module Markdown.Editor where

import Prelude
import React (ReactClass, ReactElement, unsafeCreateElement)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)


foreign import editorImpl :: forall a. ReactClass a


type RequiredProps o =
  ( defaultValue :: String
  , onChange :: EffectFn1 (Effect String) Unit
  | o )



editor :: Record (RequiredProps ()) -> ReactElement
editor props = unsafeCreateElement editorImpl props []
