module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Import (ImportDialog (..)) as Import
import Components.Dialogs.Export (exportDialog)
import Components.Snackbar (snackbars, SnackbarContent)
import Timeline.Data.TimelineName (TimelineName, initialTimelineName)
import WithRoot (withRoot)

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
import Web.File.File (File)
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Store (fileToArrayBuffer)



index :: { stateRef :: Ref Unit -- FIXME change to whatever state is changed
         }
      -> ReactElement
index {stateRef} = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          -- initialize asynchronous signals and queues
          ( importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
            ) <- IOQueues.new
          ( exportQueue :: Q.Queue (write :: Q.WRITE) ArrayBuffer
            ) <- Q.writeOnly <$> Q.new
          ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
            ) <- Q.writeOnly <$> Q.new
          -- TODO make a title and filename edit dialog
          ( nameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
            ) <- IOQueues.new
          -- status of the title and filename in the AppBar
          ( nameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
            ) <- IxSig.make initialTimelineName

          -- handlers for appbar buttons
          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              onImport :: Effect Unit
              onImport = runAff_ resolve $ do
                mFile <- IOQueues.callAsync importQueues Import.Open -- invoke opener
                case mFile of
                  Nothing -> pure unit
                  Just file -> do
                    -- TODO reconcile failure to parse with a `try` and throw a snackbar
                    buffer <- fileToArrayBuffer file
                    -- TODO decode to content state, assign to content signal
                    -- TODO assign new filename and timelineName to signal
                    liftEffect $ do
                      log $ unsafeCoerce buffer
                      -- TODO close modal externally or throw snackbar and stop loader

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
              , snackbars snackbarQueue
              ]
            }
