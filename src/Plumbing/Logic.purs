module Plumbing.Logic where

import Components.Dialogs.Import (ImportDialog(..)) as Import
import Components.Dialogs.Export (ExportDialog(..)) as Export
import Components.Dialogs.DangerConfirm (DangerConfirm(..))
import Components.Dialogs.NewOrEditTimeline (NewOrEditTimelineResult(..))
import Components.Snackbar (SnackbarContent, SnackbarVariant(Warning))
import Timeline.UI.TimeSpaceName
  (TimeSpaceName(..), clearTimeSpaceNameCache, setDefaultTimeSpaceName)
import Timeline.UI.TimeScale
  (TimeScale, clearTimeScaleCache, setDefaultTimeScale)
import Timeline.UI.Timeline (Timeline(..))
import Timeline.UI.Timelines (Timelines(..))
import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (snoc, (!!), updateAt, deleteAt) as Array
import Data.ArrayBuffer.Class (encodeArrayBuffer)
import Data.Time.Duration (Milliseconds(..))
import Data.IxSet.Demi (Index)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.DOM (text, strong)
import React.DOM.NonBlockingSpace (nbsp)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (body1)
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE, allowWriting) as Q
import IOQueues (IOQueues(..))
import IOQueues (callAsync) as IOQueues
import Zeta.Types (WRITE, READ) as S
import IxZeta (IxSignal, setDiff, get, set) as IxSig
import Web.File.File (File)
import Web.File.Store (fileToArrayBuffer)
import Unsafe.Coerce (unsafeCoerce)

-- | When the "Import" button is clicked
onImport ::
  { importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
  , timeSpaceNameSignal :: IxSig.IxSignal ( read :: S.READ ) TimeSpaceName
  , settingsSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Settings
  } ->
  Effect Unit
onImport { importQueues
, timeSpaceNameSignal
, settingsSignal
} =
  launchAff_ do
    mFile <- IOQueues.callAsync importQueues Import.Open -- invoke opener
    case mFile of
      Nothing -> pure unit
      Just file -> do
        liftEffect do
          -- assign the filename
          TimeSpaceName timeSpaceName <- IxSig.get timeSpaceNameSignal
          -- FIXME filename
          -- IxSig.setDiff (TimeSpaceName $ timeSpaceName {filename = File.name file}) timeSpaceNameSignal
          -- reset settings to be read-only
          Settings settings <- IxSig.get settingsSignal
          IxSig.setDiff (Settings $ settings { isEditable = false }) settingsSignal
        -- TODO reconcile failure to parse with a `try` and throw a snackbar
        -- TODO decode to content state, assign to content signal
        buffer <- fileToArrayBuffer file
        liftEffect do
          log $ unsafeCoerce buffer
          -- TODO throw snackbar and stop loader if failing
          case importQueues of
            IOQueues { input } -> Q.put (Q.allowWriting input) Import.Close

-- | When opening the "Export" window
onExport ::
  { exportQueue :: Q.Queue ( write :: Q.WRITE ) Export.ExportDialog
  } ->
  Effect Unit
onExport { exportQueue } = do
  -- TODO encode actual content state from content signal
  buffer <- encodeArrayBuffer "yo dawg"
  -- FIXME get filename state
  -- TimeSpaceName {filename} <- IxSig.get timeSpaceNameSignal
  Q.put exportQueue (Export.ExportDialog { buffer, filename: "foobar" })

-- | Clears local unsaved cache, and triggers a snackbar message
onClickedExport ::
  { snackbarQueue :: Q.Queue ( write :: Q.WRITE ) SnackbarContent
  } ->
  Effect Unit
onClickedExport { snackbarQueue } = do
  clearTimeSpaceNameCache
  clearTimeScaleCache
  Q.put snackbarQueue
    { variant: Warning
    , message: "Local Unsaved Data Cache Deleted"
    , timeout: Just (Milliseconds 5000.0)
    }

-- | open new timeline
onNew ::
  { dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , timeScaleSignal :: IxSig.IxSignal ( write :: S.WRITE ) TimeScale
  , timeSpaceNameSignal :: IxSig.IxSignal ( write :: S.WRITE ) TimeSpaceName
  } ->
  Effect Unit
onNew { dangerConfirmQueues, timeScaleSignal, timeSpaceNameSignal } =
  launchAff_ do
    resetAll <-
      IOQueues.callAsync dangerConfirmQueues
        $ DangerConfirm
            { content:
                [ typography
                    { variant: body1
                    }
                    [ strong [] [ text "Warning!" ]
                    , nbsp
                    , text "Creating a new document will erase all content! You can keep any unsaved work with the \"Export\" button."
                    ]
                ]
            , submit: "Create New Document"
            }
    when resetAll
      $ liftEffect do
          setDefaultTimeScale timeScaleSignal
          setDefaultTimeSpaceName timeSpaceNameSignal

-- | Invokes dialog queues and stores the result in state signals
onTimeSpaceNameEdit ::
  { timeSpaceNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimeSpaceName)
  , timeSpaceNameSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpaceName
  } ->
  Effect Unit
onTimeSpaceNameEdit { timeSpaceNameEditQueues, timeSpaceNameSignal } =
  launchAff_ do
    mEditedTimeSpaceName <- IOQueues.callAsync timeSpaceNameEditQueues unit
    case mEditedTimeSpaceName of
      Nothing -> pure unit
      Just newTimeSpaceName -> liftEffect (IxSig.setDiff newTimeSpaceName timeSpaceNameSignal)

onTimeScaleEdit ::
  { timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
  , timeScaleSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) TimeScale
  } ->
  Effect Unit
onTimeScaleEdit { timeScaleEditQueues, timeScaleSignal } =
  launchAff_ do
    mEditedTimeScale <- IOQueues.callAsync timeScaleEditQueues unit
    case mEditedTimeScale of
      Nothing -> pure unit
      Just newTimeScale -> liftEffect (IxSig.setDiff newTimeScale timeScaleSignal)

onSettingsEdit ::
  { settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Settings
  } ->
  Effect Unit
onSettingsEdit { settingsEditQueues, settingsSignal } =
  launchAff_ do
    mEditedSettings <- IOQueues.callAsync settingsEditQueues unit
    case mEditedSettings of
      Nothing -> pure unit
      Just newSettings -> liftEffect (IxSig.setDiff newSettings settingsSignal)

onReadEULA :: { eulaQueue :: Q.Queue ( write :: Q.WRITE ) Unit } -> Effect Unit
onReadEULA { eulaQueue } = Q.put eulaQueue unit

onExploreTimeSpaces ::
  { exploreTimeSpacesQueues :: IOQueues Q.Queue Unit (Maybe (Array Index))
  , timeSpaceSelectedSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) (Array Index)
  -- TODO assign new timespace name, pulling from the global directory, not the explore sub-view
  -- , timeSpaceNameSignal :: IxSig.IxSignal (write :: S.WRITE) TimeSpaceName
  -- , exploreTimeSpacesSignal :: IxSig.IxSignal (read :: S.READ) (WithSpanOfTime ExploreTimeSpaces)
  } ->
  Effect Unit
onExploreTimeSpaces { exploreTimeSpacesQueues, timeSpaceSelectedSignal } =
  launchAff_ do
    mNewSelected <- IOQueues.callAsync exploreTimeSpacesQueues unit
    case mNewSelected of
      Nothing -> pure unit
      Just selected -> liftEffect (IxSig.setDiff selected timeSpaceSelectedSignal)

onClickedNewTimeline ::
  { newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe Timeline) (Maybe NewOrEditTimelineResult)
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  -- FIXME gonna need to scope into the timespace this belongs to, to add it and stuff
  } ->
  Effect Unit
onClickedNewTimeline { newOrEditTimelineQueues, timelinesSignal } =
  launchAff_ do
    mResult <- IOQueues.callAsync newOrEditTimelineQueues Nothing
    case mResult of
      Nothing -> pure unit
      Just result -> case result of
        NewOrEditTimeline t ->
          liftEffect do
            Timelines ts <- IxSig.get timelinesSignal
            IxSig.set (Timelines (Array.snoc ts t)) timelinesSignal
        DeleteTimeline -> liftEffect $ throw "Shouldn't be possible!"

onClickedEditTimeline ::
  { newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe Timeline) (Maybe NewOrEditTimelineResult)
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  } ->
  Int -> Effect Unit
onClickedEditTimeline { newOrEditTimelineQueues, timelinesSignal } index = do
  Timelines timelines <- IxSig.get timelinesSignal
  case timelines Array.!! index of
    Nothing -> throw $ "Timeline does not exist in set: " <> show index
    Just t ->
      launchAff_ do
        mResult <- IOQueues.callAsync newOrEditTimelineQueues (Just t)
        case mResult of
          Nothing -> pure unit
          Just result ->
            liftEffect
              $ case result of
                  NewOrEditTimeline t -> case Array.updateAt index t timelines of
                    Nothing -> throw $ "Timeline index does not exist in set: " <> show index
                    Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal
                  DeleteTimeline -> case Array.deleteAt index timelines of
                    Nothing -> throw $ "Timeline index does not exist in set: " <> show index
                    Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal

onClickedDeleteTimeline ::
  { dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  } ->
  Either Int (Effect Unit) -> Effect Unit
onClickedDeleteTimeline { dangerConfirmQueues, timelinesSignal } eIndexAuxAction =
  launchAff_ do
    confirmed <-
      IOQueues.callAsync dangerConfirmQueues
        $ DangerConfirm
            { content:
                [ typography { variant: body1 }
                    [ strong [] [ text "Warning!" ]
                    , nbsp
                    , text "Are you sure you want to delete this timeline?"
                    ]
                ]
            , submit: "Delete Timeline"
            }
    when confirmed $ liftEffect
      $ case eIndexAuxAction of
          Left index -> do
            Timelines timelines <- IxSig.get timelinesSignal
            case Array.deleteAt index timelines of
              Nothing -> throw $ "Timeline index does not exist in set: " <> show index
              Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal
          Right auxAction -> auxAction
