module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import WithRoot (withRoot)
import Stream.Response (newResponse, getArrayBuffer)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Ref (Ref)
import Queue.One (new, put) as Q
import Queue.Types (writeOnly) as Q
import IOQueues (new, callAsync) as IOQueues
import Web.File.File (toBlob)
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)



index :: { stateRef :: Ref Unit
         }
      -> ReactElement
index {stateRef} = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          importQueues <- IOQueues.new
          exportQueue <- Q.writeOnly <$> Q.new

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              onImport :: Effect Unit
              onImport = runAff_ resolve $ do
                mFile <- IOQueues.callAsync importQueues unit
                case mFile of
                  Nothing -> pure unit
                  Just file -> do
                    let blob = toBlob file
                    resp <- liftEffect (newResponse blob)
                    buffer <- getArrayBuffer resp
                    -- TODO decode to content state
                    -- TODO somehow get the actual filename?
                    liftEffect $ do
                      log $ unsafeCoerce buffer

              onExport :: Effect Unit
              onExport = do
                -- TODO encode actual content state
                -- TODO grab filename from actual uploaded, make editable
                -- - specifically, the filename should first reflect the title (camelcased), unless uploaded or decided
                b <- encodeArrayBuffer "yo dawg"
                Q.put exportQueue b
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar {onImport, onExport}
              , typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , importDialog importQueues
              , exportDialog exportQueue
              ]
            }
