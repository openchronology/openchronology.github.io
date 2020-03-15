module Components.Index where

import Components.TopBar (topBar)
import Components.BottomBar (bottomBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Import (ImportDialog (..)) as Import
import Components.Dialogs.Export (exportDialog)
import Components.Dialogs.Export (ExportDialog (..)) as Export
import Components.Snackbar (snackbars, SnackbarContent)
import Timeline.Data.TimelineName (TimelineName, initialTimelineName)
import Timeline.Data.TimeScale (TimeScale, initialTimeScale)
import WithRoot (withRoot)

import Prelude hiding (div)
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Ref (Ref)
import Queue.One (Queue, new, put) as Q
import Queue.Types (writeOnly, allowReading, readOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new, callAsync) as IOQueues
import Signal.Types (WRITE, READ, readOnly) as S
import IxSignal (IxSignal, make, setDiff) as IxSig
import Web.File.File (File)
import Web.File.Store (fileToArrayBuffer)
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (style) as P
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)



index :: { stateRef :: Ref Unit -- FIXME change to whatever state is changed - is this even needed?
         }
      -> ReactElement
index {stateRef} = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          -- initialize asynchronous signals and queues
          -- dialog queues
          ( importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
            ) <- IOQueues.new
          ( exportQueue :: Q.Queue (write :: Q.WRITE) Export.ExportDialog
            ) <- Q.writeOnly <$> Q.new
          -- TODO make a title and filename edit dialog
          ( timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
            ) <- IOQueues.new
          ( timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
            ) <- IOQueues.new

          -- sudden messages and notices
          ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
            ) <- Q.writeOnly <$> Q.new

          -- shared state signals
          -- TODO originate source of data from IndexedDB? File uploading _sets_ these signals
          -- status of the title and filename in the TopBar
          ( timelineNameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
            ) <- IxSig.make initialTimelineName
          -- status of the timescale in the BottomBar
          ( timeScaleSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimeScale
            ) <- IxSig.make initialTimeScale
          -- initial zoom level
          ( zoomSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Number
            ) <- IxSig.make 100.0

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
                    -- TODO store filename and title into TimelineName data
                    -- TODO assign new filename and timelineName to signal
                    -- TODO reconcile failure to parse with a `try` and throw a snackbar
                    -- TODO decode to content state, assign to content signal
                    buffer <- fileToArrayBuffer file
                    liftEffect $ do
                      log $ unsafeCoerce buffer
                      -- TODO close modal externally or throw snackbar and stop loader

              onExport :: Effect Unit
              onExport = do
                -- TODO encode actual content state from content signal
                -- TODO grab filename from signal
                -- - specifically, the filename should first reflect the title (camelcased),
                --   unless uploaded or decided
                buffer <- encodeArrayBuffer "yo dawg"
                Q.put exportQueue (Export.ExportDialog {buffer, filename: "foo.och"})

              -- patching between dialog queues and state signals
              onTimelineNameEdit :: Effect Unit
              onTimelineNameEdit = runAff_ resolve $ do
                mEditedTimelineName <- IOQueues.callAsync timelineNameEditQueues unit
                case mEditedTimelineName of
                  Nothing -> pure unit
                  Just newTimelineName -> liftEffect (IxSig.setDiff newTimelineName timelineNameSignal)

              onTimeScaleEdit :: Effect Unit
              onTimeScaleEdit = runAff_ resolve $ do
                mEditedTimeScale <- IOQueues.callAsync timeScaleEditQueues unit
                case mEditedTimeScale of
                  Nothing -> pure unit
                  Just newTimeScale -> liftEffect (IxSig.setDiff newTimeScale timeScaleSignal)
          pure
            { state: {}
            , render: pure $ toElement
              [ topBar
                { onImport
                , onExport
                , onTimelineNameEdit
                , timelineNameSignal: S.readOnly timelineNameSignal
                }
              , div [P.style {height: "100%", padding: "3em 0"}]
                [ typography {gutterBottom: true, variant: title} [text "Just a Test"]
                ]
              , bottomBar
                { onTimeScaleEdit
                , zoomSignal: S.readOnly zoomSignal
                , timeScaleSignal: S.readOnly timeScaleSignal
                }
              , importDialog importQueues
              , exportDialog (Q.readOnly (Q.allowReading exportQueue))
              , snackbars snackbarQueue
              ]
            }
