module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import WithRoot (withRoot)

import Prelude
import Data.Either (Either (..))
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import IOQueues (new, callAsync) as Q
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)


index :: ReactElement
index = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          qs <- Q.new
          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit
              onImport = runAff_ resolve $ do
                mF <- Q.callAsync qs unit
                liftEffect $ log $ unsafeCoerce mF
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar {onImport, onExport: pure unit}
              , typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , importDialog qs
              ]
            }
