module Plumbing.Logic where

import Components.Dialogs.Import (ImportDialog (..)) as Import
import Components.Dialogs.Export (ExportDialog (..)) as Export
import Components.Snackbar (SnackbarContent, SnackbarVariant (Warning))
import Timeline.Data.TimelineName
  (TimelineName (..), clearTimelineNameCache, setDefaultTimelineName)
import Timeline.Data.TimeScale
  (TimeScale, clearTimeScaleCache, setDefaultTimeScale)
import Settings (Settings (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Class (encodeArrayBuffer)
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE, allowWriting) as Q
import IOQueues (IOQueues (..))
import IOQueues (callAsync) as IOQueues
import Zeta.Types (WRITE, READ) as S
import IxZeta (IxSignal, setDiff, get) as IxSig
import Web.File.File (File)
import Web.File.Store (fileToArrayBuffer)
import Unsafe.Coerce (unsafeCoerce)



-- | When the "Import" button is clicked
onImport :: { importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
            , timelineNameSignal :: IxSig.IxSignal (read :: S.READ) TimelineName
            , settingsSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Settings
            }
         -> Effect Unit
onImport
  { importQueues
  , timelineNameSignal
  , settingsSignal
  } = launchAff_ do
  mFile <- IOQueues.callAsync importQueues Import.Open -- invoke opener
  case mFile of
    Nothing -> pure unit
    Just file -> do
      liftEffect do
        -- assign the filename
        TimelineName timelineName <- IxSig.get timelineNameSignal
        -- FIXME filename
        -- IxSig.setDiff (TimelineName $ timelineName {filename = File.name file}) timelineNameSignal

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


-- | When opening the "Export" window
onExport :: { exportQueue :: Q.Queue (write :: Q.WRITE) Export.ExportDialog
            }
         -> Effect Unit
onExport {exportQueue} = do
  -- TODO encode actual content state from content signal
  buffer <- encodeArrayBuffer "yo dawg"
  -- FIXME get filename state
  -- TimelineName {filename} <- IxSig.get timelineNameSignal
  Q.put exportQueue (Export.ExportDialog {buffer, filename: "foobar"})


-- | Clears local unsaved cache, and triggers a snackbar message
onClickedExport :: { snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
                   }
                -> Effect Unit
onClickedExport {snackbarQueue} = do
  clearTimelineNameCache
  clearTimeScaleCache
  Q.put snackbarQueue
      { variant: Warning
      , message: "Local Unsaved Data Cache Deleted"
      , timeout: Just (Milliseconds 5000.0)
      }


-- | open new timeline
onNew :: { newQueues :: IOQueues Q.Queue Unit Boolean
         , timeScaleSignal :: IxSig.IxSignal (write :: S.WRITE) TimeScale
         , timelineNameSignal :: IxSig.IxSignal (write :: S.WRITE) TimelineName
         }
      -> Effect Unit
onNew {newQueues,timeScaleSignal,timelineNameSignal} = launchAff_ do
  resetAll <- IOQueues.callAsync newQueues unit
  when resetAll $ liftEffect do
    setDefaultTimeScale timeScaleSignal
    setDefaultTimelineName timelineNameSignal


-- | Invokes dialog queues and stores the result in state signals
onTimelineNameEdit :: { timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
                      , timelineNameSignal :: IxSig.IxSignal (read :: S.READ, write :: S.WRITE) TimelineName
                      }
                   -> Effect Unit
onTimelineNameEdit {timelineNameEditQueues,timelineNameSignal} = launchAff_ do
  mEditedTimelineName <- IOQueues.callAsync timelineNameEditQueues unit
  case mEditedTimelineName of
    Nothing -> pure unit
    Just newTimelineName -> liftEffect (IxSig.setDiff newTimelineName timelineNameSignal)


onTimeScaleEdit :: { timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
                   , timeScaleSignal :: IxSig.IxSignal (read :: S.READ, write :: S.WRITE) TimeScale
                   }
                -> Effect Unit
onTimeScaleEdit {timeScaleEditQueues,timeScaleSignal} = launchAff_ do
  mEditedTimeScale <- IOQueues.callAsync timeScaleEditQueues unit
  case mEditedTimeScale of
    Nothing -> pure unit
    Just newTimeScale -> liftEffect (IxSig.setDiff newTimeScale timeScaleSignal)


onSettingsEdit :: { settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
                  , settingsSignal :: IxSig.IxSignal (read :: S.READ, write :: S.WRITE) Settings
                  }
               -> Effect Unit
onSettingsEdit {settingsEditQueues,settingsSignal} = launchAff_ do
  mEditedSettings <- IOQueues.callAsync settingsEditQueues unit
  case mEditedSettings of
    Nothing -> pure unit
    Just newSettings -> liftEffect (IxSig.setDiff newSettings settingsSignal)
