module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import Timeline.Data.TimelineName (TimelineName, initialTimelineName)
import WithRoot (withRoot)
import Stream.Response (newResponse, getArrayBuffer)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Ref (Ref)
import Queue.One (Queue, new, put) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new, callAsync) as IOQueues
import Signal.Types (WRITE, READ) as S
import IxSignal (IxSignal, make, setDiff) as IxSig
import Web.File.File (File, toBlob)
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
          -- initialize asynchronous signals and queues
          ( importQueues :: IOQueues Q.Queue Unit (Maybe File)
            ) <- IOQueues.new
          ( exportQueue :: Q.Queue (write :: Q.WRITE) ArrayBuffer
            ) <- Q.writeOnly <$> Q.new
          ( nameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
            ) <- IOQueues.new
          ( nameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
            ) <- IxSig.make initialTimelineName

          -- handlers for appbar buttons
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
                    -- TODO decode to content state, assign to content signal
                    -- TODO assign new filename and timelineName to signal
                    liftEffect $ do
                      log $ unsafeCoerce buffer
                    -- TODO close modal externally?

              onExport :: Effect Unit
              onExport = do
                -- TODO encode actual content state from content signal
                -- TODO grab filename from signal, make editable
                -- - specifically, the filename should first reflect the title (camelcased),
                --   unless uploaded or decided
                b <- encodeArrayBuffer "yo dawg"
                Q.put exportQueue b

              onNameEdit :: Effect Unit
              onNameEdit = runAff_ resolve $ do
                mEditedName <- IOQueues.callAsync nameEditQueues unit
                case mEditedName of
                  Nothing -> pure unit
                  Just newTimelineName -> liftEffect (IxSig.setDiff newTimelineName nameSignal)
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar {onImport, onExport, onNameEdit}
              , typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , importDialog importQueues
              , exportDialog exportQueue
              ]
            }
