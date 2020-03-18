module Components.Index where

import Components.TopBar (topBar)
import Components.BottomBar (bottomBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Import (ImportDialog (..)) as Import
import Components.Dialogs.Export (exportDialog)
import Components.Dialogs.Export (ExportDialog (..)) as Export
import Components.Dialogs.New (newDialog)
import Components.Dialogs.TimelineNameEdit (timelineNameEditDialog)
import Components.Dialogs.TimeScaleEdit (timeScaleEditDialog)
import Components.Dialogs.SettingsEdit (settingsEditDialog)
import Components.Snackbar (snackbars, SnackbarContent, SnackbarVariant (Warning))
import Timeline.Data.TimelineName
  (TimelineName (..), newTimelineNameSignal, clearTimelineNameCache, setDefaultTimelineName)
import Timeline.Data.TimeScale
  (TimeScale, newTimeScaleSignal, clearTimeScaleCache, setDefaultTimeScale)
import Settings (Settings (..), newSettingsSignal)
import WithRoot (withRoot)

import Prelude hiding (div)
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Ref (Ref)
import Queue.One (Queue, new, put) as Q
import Queue.Types (allowWriting, writeOnly, allowReading, readOnly, WRITE) as Q
import IOQueues (IOQueues (..))
import IOQueues (new, callAsync) as IOQueues
import Signal.Types (WRITE, READ, readOnly, writeOnly) as S
import IxSignal (IxSignal, make, setDiff, get) as IxSig
import Web.File.File (File)
import Web.File.File (name) as File
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
          ( newQueues :: IOQueues Q.Queue Unit Boolean
            ) <- IOQueues.new
          ( settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
            ) <- IOQueues.new
          ( timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
            ) <- IOQueues.new
          ( timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
            ) <- IOQueues.new

          -- sudden messages and notices
          ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
            ) <- Q.writeOnly <$> Q.new

          -- shared state signals
          ( settingsSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Settings
            ) <- newSettingsSignal {wasOpenedByShareLink: false} -- FIXME
          -- status of the title and filename in the TopBar
          ( timelineNameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
            ) <- newTimelineNameSignal (S.readOnly settingsSignal)
          -- status of the timescale in the BottomBar
          ( timeScaleSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimeScale
            ) <- newTimeScaleSignal (S.readOnly settingsSignal)
          -- initial zoom level
          ( zoomSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Number
            ) <- IxSig.make 100.0

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              onImport :: Effect Unit
              onImport = runAff_ resolve do
                mFile <- IOQueues.callAsync importQueues Import.Open -- invoke opener
                case mFile of
                  Nothing -> pure unit
                  Just file -> do
                    liftEffect do
                      -- assign the filename
                      TimelineName timelineName <- IxSig.get timelineNameSignal
                      IxSig.setDiff (TimelineName $ timelineName {filename = File.name file}) timelineNameSignal

                      -- reset settings to be read-only
                      Settings settings <- IxSig.get settingsSignal
                      IxSig.setDiff (Settings $ settings {isEditable = false}) settingsSignal

                    -- TODO reconcile failure to parse with a `try` and throw a snackbar
                    -- TODO decode to content state, assign to content signal
                    buffer <- fileToArrayBuffer file
                    liftEffect do
                      log $ unsafeCoerce buffer
                      -- TODO throw snackbar and stop loader if failing
                      case importQueues of
                        IOQueues {input} -> Q.put (Q.allowWriting input) Import.Close

              onExport :: Effect Unit
              onExport = do
                -- TODO encode actual content state from content signal
                buffer <- encodeArrayBuffer "yo dawg"
                TimelineName {filename} <- IxSig.get timelineNameSignal
                Q.put exportQueue (Export.ExportDialog {buffer, filename})

              -- new timeline
              onNew :: Effect Unit
              onNew = runAff_ resolve do
                resetAll <- IOQueues.callAsync newQueues unit
                when resetAll $ liftEffect do
                  setDefaultTimeScale (S.writeOnly timeScaleSignal)
                  setDefaultTimelineName (S.writeOnly timelineNameSignal)

              -- clears local unsaved cache, and triggers a snackbar message
              onClickedExport :: Effect Unit
              onClickedExport = do
                clearTimelineNameCache
                clearTimeScaleCache
                Q.put snackbarQueue
                  { variant: Warning
                  , message: "Local Unsaved Data Cache Deleted"
                  , timeout: Just (Milliseconds 5000.0)
                  }

              -- invokes dialog queues and stores the result in state signals
              onTimelineNameEdit :: Effect Unit
              onTimelineNameEdit = runAff_ resolve do
                mEditedTimelineName <- IOQueues.callAsync timelineNameEditQueues unit
                case mEditedTimelineName of
                  Nothing -> pure unit
                  Just newTimelineName -> liftEffect (IxSig.setDiff newTimelineName timelineNameSignal)

              onTimeScaleEdit :: Effect Unit
              onTimeScaleEdit = runAff_ resolve do
                mEditedTimeScale <- IOQueues.callAsync timeScaleEditQueues unit
                case mEditedTimeScale of
                  Nothing -> pure unit
                  Just newTimeScale -> liftEffect (IxSig.setDiff newTimeScale timeScaleSignal)

              onSettingsEdit :: Effect Unit
              onSettingsEdit = runAff_ resolve do
                mEditedSettings <- IOQueues.callAsync settingsEditQueues unit
                case mEditedSettings of
                  Nothing -> pure unit
                  Just newSettings -> liftEffect (IxSig.setDiff newSettings settingsSignal)
          pure
            { state: {}
            , render: pure $ toElement
              [ topBar
                { onImport
                , onExport
                , onNew
                , onTimelineNameEdit
                , onSettingsEdit
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

              -- dialogs
              , importDialog importQueues
              , exportDialog
                { exportQueue: Q.readOnly (Q.allowReading exportQueue)
                , onClickedExport
                }
              , newDialog {newQueues}
              , timelineNameEditDialog
                { timelineNameSignal: S.readOnly timelineNameSignal
                , settingsSignal: S.readOnly settingsSignal
                , timelineNameEditQueues
                }
              , timeScaleEditDialog
                { timeScaleSignal: S.readOnly timeScaleSignal
                , settingsSignal: S.readOnly settingsSignal
                , timeScaleEditQueues
                }
              , settingsEditDialog
                { settingsSignal: S.readOnly settingsSignal
                , settingsEditQueues
                }
              , snackbars (Q.readOnly (Q.allowReading snackbarQueue))
              ]
            }
