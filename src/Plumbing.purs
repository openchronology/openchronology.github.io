module Plumbing where

{-|

This module defines the primary async storage and relaying mechanisms used throughout the user interface.

Generally speaking, Queues are used for opening dialogs and triggering an "ask" for input, while
Signals represent a shared state used throughout multiple UI components, that updates the component
when its state has been changed.

Furthermore, there is some logic defined here where it's ambiguous to the user interface, specifically,
but still creates desirable effects (i.e. triggering an ask for data, then piping that data into a state).

-}

import Components.Dialogs.Import (ImportDialog (..)) as Import
import Components.Dialogs.Export (ExportDialog (..)) as Export
import Timeline.Data.TimelineName
  (TimelineName (..), newTimelineNameSignal, clearTimelineNameCache, setDefaultTimelineName)
import Timeline.Data.TimeScale
  (TimeScale, newTimeScaleSignal, clearTimeScaleCache, setDefaultTimeScale)
import Components.Snackbar (SnackbarContent, SnackbarVariant (Warning))
import Settings (Settings (..), newSettingsSignal)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.ArrayBuffer.Class (encodeArrayBuffer)
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Queue.One (Queue, new, put) as Q
import Queue.Types (allowWriting, writeOnly, WRITE) as Q
import IOQueues (IOQueues (..))
import IOQueues (new, callAsync) as IOQueues
import Zeta.Types (WRITE, READ, readOnly, writeOnly) as S
import IxZeta (IxSignal, make, setDiff, get) as IxSig
import Web.File.File (File)
import Web.File.Store (fileToArrayBuffer)
import Unsafe.Coerce (unsafeCoerce)


-- | Mostly just Dialog invocations
type PrimaryQueues =
  { importQueues           :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
  , exportQueue            :: Q.Queue (write :: Q.WRITE) Export.ExportDialog
  , newQueues              :: IOQueues Q.Queue Unit Boolean
  , settingsEditQueues     :: IOQueues Q.Queue Unit (Maybe Settings)
  , timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
  , timeScaleEditQueues    :: IOQueues Q.Queue Unit (Maybe TimeScale)
  , snackbarQueue          :: Q.Queue (write :: Q.WRITE) SnackbarContent
  }


-- | Created only on boot of the program
newPrimaryQueues :: Effect PrimaryQueues
newPrimaryQueues = do
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

  pure
    { importQueues
    , exportQueue
    , newQueues
    , settingsEditQueues
    , timelineNameEditQueues
    , timeScaleEditQueues
    , snackbarQueue
    }


-- | shared state signals
type PrimarySignals =
  { settingsSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Settings
  -- status of the title and filename in the TopBar
  , timelineNameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
  -- status of the timescale in the BottomBar
  , timeScaleSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimeScale
  -- initial zoom level
  , zoomSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Number
  }


-- | Created only on boot of the program
newPrimarySignals :: Effect PrimarySignals
newPrimarySignals = do
  ( settingsSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Settings
    ) <- newSettingsSignal {wasOpenedByShareLink: false} -- FIXME bind to share link, if opened by one
  -- status of the title and filename in the TopBar
  ( timelineNameSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimelineName
    ) <- newTimelineNameSignal (S.readOnly settingsSignal)
  -- status of the timescale in the BottomBar
  ( timeScaleSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) TimeScale
    ) <- newTimeScaleSignal (S.readOnly settingsSignal)
  -- initial zoom level
  ( zoomSignal :: IxSig.IxSignal (write :: S.WRITE, read :: S.READ) Number
    ) <- IxSig.make 100.0

  pure
    { settingsSignal
    , timelineNameSignal
    , timeScaleSignal
    , zoomSignal
    }


-- | Functions given to the React.js components, to interact with the async devices.
type LogicFunctions =
  { onImport           :: Effect Unit
  , onExport           :: Effect Unit
  , onClickedExport    :: Effect Unit
  , onNew              :: Effect Unit
  , onTimelineNameEdit :: Effect Unit
  , onTimeScaleEdit    :: Effect Unit
  , onSettingsEdit     :: Effect Unit
  }


-- | Create the logical functions
logic :: PrimaryQueues -> PrimarySignals -> LogicFunctions
logic
  { importQueues
  , exportQueue
  , newQueues
  , settingsEditQueues
  , timelineNameEditQueues
  , timeScaleEditQueues
  , snackbarQueue
  }
  { settingsSignal
  , timelineNameSignal
  , timeScaleSignal
  , zoomSignal
  } =
  { onImport
  , onExport
  , onClickedExport
  , onNew
  , onTimelineNameEdit
  , onTimeScaleEdit
  , onSettingsEdit
  }
  where
    -- | When the "Import" button is clicked
    onImport :: Effect Unit
    onImport = runAff_ resolve do
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
    onExport :: Effect Unit
    onExport = do
      -- TODO encode actual content state from content signal
      buffer <- encodeArrayBuffer "yo dawg"
      -- FIXME get filename state
      -- TimelineName {filename} <- IxSig.get timelineNameSignal
      Q.put exportQueue (Export.ExportDialog {buffer, filename: "foobar"})

    -- | Clears local unsaved cache, and triggers a snackbar message
    onClickedExport :: Effect Unit
    onClickedExport = do
      clearTimelineNameCache
      clearTimeScaleCache
      Q.put snackbarQueue
        { variant: Warning
        , message: "Local Unsaved Data Cache Deleted"
        , timeout: Just (Milliseconds 5000.0)
        }


    -- | open new timeline
    onNew :: Effect Unit
    onNew = runAff_ resolve do
      resetAll <- IOQueues.callAsync newQueues unit
      when resetAll $ liftEffect do
        setDefaultTimeScale (S.writeOnly timeScaleSignal)
        setDefaultTimelineName (S.writeOnly timelineNameSignal)


    -- | Invokes dialog queues and stores the result in state signals
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


    resolve eX = case eX of
      Left err -> throwException err
      Right _ -> pure unit
