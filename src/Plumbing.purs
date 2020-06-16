{-
OpenChronology - an application for graphing and visualizing timelines.
Copyright (C) 2020  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module Plumbing where

{-|

This module defines the primary async storage and relaying mechanisms used throughout the user interface.

Generally speaking, Queues are used for opening dialogs and triggering an "ask" for input, while
Signals represent a shared state used throughout multiple UI components, that updates the component
when its state has been changed.

Furthermore, there is some logic defined here where it's ambiguous to the user interface, specifically,
but still creates desirable effects (i.e. triggering an ask for data, then piping that data into a state).

-}
import Plumbing.Logic
  ( onImport
  , onExport
  , onClickedExport
  , onNew
  , onTimeSpaceNameEdit
  , onTimeScaleEdit
  , onSettingsEdit
  , onReadEULA
  , onExploreTimeSpaces
  , onClickedNewTimeline
  , onClickedEditTimeline
  , onClickedDeleteTimeline
  )
import Components.Dialogs.Import (ImportDialog) as Import
import Components.Dialogs.Export (ExportDialog) as Export
import Components.Dialogs.DangerConfirm (DangerConfirm)
import Components.Dialogs.NewOrEditTimeline (NewOrEditTimelineResult)
import Components.Snackbar (SnackbarContent)
import Timeline.UI.TimeSpaceName (TimeSpaceName, newTimeSpaceNameSignal)
import Timeline.UI.TimeScale (TimeScale, newTimeScaleSignal)
import Timeline.UI.ExploreTimeSpaces (ExploreTimeSpaces, WithSpanOfTime, newExploreTimeSpacesSignal)
import Timeline.UI.Timeline (Timeline)
import Timeline.UI.Timelines (Timelines, newTimelinesSignal)
import Timeline.UI.Siblings (Siblings, newSiblingsSignal)
import Timeline.UI.Children (Children, newChildrenSignal)
import Settings (Settings, newSettingsSignal)
import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.IxSet.Demi (Index)
import Effect (Effect)
import Queue.One (Queue, new) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new) as IOQueues
import Zeta.Types (WRITE, READ, readOnly, writeOnly) as S
import IxZeta (IxSignal, make) as IxSig
import Web.File.File (File)

-- | Mostly just Dialog invocations
type PrimaryQueues
  = { importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
    , exportQueue :: Q.Queue ( write :: Q.WRITE ) Export.ExportDialog
    , settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
    , timeSpaceNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimeSpaceName)
    , timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
    , snackbarQueue :: Q.Queue ( write :: Q.WRITE ) SnackbarContent
    , eulaQueue :: Q.Queue ( write :: Q.WRITE ) Unit
    , exploreTimeSpacesQueues :: IOQueues Q.Queue Unit (Maybe (Array Index))
    , dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
    , newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe Timeline) (Maybe NewOrEditTimelineResult)
    }

-- | Created only on boot of the program
newPrimaryQueues :: Effect PrimaryQueues
newPrimaryQueues = do
  ( importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
  ) <-
    IOQueues.new
  ( exportQueue :: Q.Queue ( write :: Q.WRITE ) Export.ExportDialog
  ) <-
    Q.writeOnly <$> Q.new
  ( settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
  ) <-
    IOQueues.new
  ( timeSpaceNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimeSpaceName)
  ) <-
    IOQueues.new
  ( timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
  ) <-
    IOQueues.new
  -- sudden messages and notices
  ( snackbarQueue :: Q.Queue ( write :: Q.WRITE ) SnackbarContent
  ) <-
    Q.writeOnly <$> Q.new
  ( eulaQueue :: Q.Queue ( write :: Q.WRITE ) Unit
  ) <-
    Q.writeOnly <$> Q.new
  ( exploreTimeSpacesQueues :: IOQueues Q.Queue Unit (Maybe (Array Index))
  ) <-
    IOQueues.new
  ( dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  ) <-
    IOQueues.new
  ( newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe Timeline) (Maybe NewOrEditTimelineResult)
  ) <-
    IOQueues.new
  pure
    { importQueues
    , exportQueue
    , settingsEditQueues
    , timeSpaceNameEditQueues
    , timeScaleEditQueues
    , snackbarQueue
    , eulaQueue
    , exploreTimeSpacesQueues
    , dangerConfirmQueues
    , newOrEditTimelineQueues
    }

-- | shared state signals
type PrimarySignals
  = { settingsSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Settings
    -- status of the title and filename in the TopBar
    , timeSpaceNameSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) TimeSpaceName
    -- status of the timescale in the BottomBar
    , timeScaleSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) TimeScale
    -- initial zoom level
    , zoomSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Number
    , exploreTimeSpacesSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) (WithSpanOfTime ExploreTimeSpaces)
    , timeSpaceSelectedSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) (Array Index)
    , timelinesSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Timelines
    , siblingsSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Siblings
    , childrenSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Children
    }

-- | Created only on boot of the program
newPrimarySignals :: Effect PrimarySignals
newPrimarySignals = do
  ( settingsSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Settings
  ) <-
    newSettingsSignal { wasOpenedByShareLink: false } -- FIXME bind to share link, if opened by one
  -- status of the title and filename in the TopBar
  ( timeSpaceNameSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) TimeSpaceName
  ) <-
    newTimeSpaceNameSignal (S.readOnly settingsSignal)
  -- status of the timescale in the BottomBar
  ( timeScaleSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) TimeScale
  ) <-
    newTimeScaleSignal (S.readOnly settingsSignal)
  -- initial zoom level
  ( zoomSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Number
  ) <-
    IxSig.make 100.0
  ( exploreTimeSpacesSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) (WithSpanOfTime ExploreTimeSpaces)
  ) <-
    newExploreTimeSpacesSignal
  ( timeSpaceSelectedSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) (Array Index)
  ) <-
    IxSig.make []
  ( timelinesSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Timelines
  ) <-
    newTimelinesSignal (S.readOnly settingsSignal)
  ( siblingsSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Siblings
  ) <-
    newSiblingsSignal (S.readOnly settingsSignal)
  ( childrenSignal :: IxSig.IxSignal ( write :: S.WRITE, read :: S.READ ) Children
  ) <-
    newChildrenSignal (S.readOnly settingsSignal)
  pure
    { settingsSignal
    , timeSpaceNameSignal
    , timeScaleSignal
    , zoomSignal
    , exploreTimeSpacesSignal
    , timeSpaceSelectedSignal
    , timelinesSignal
    , siblingsSignal
    , childrenSignal
    }

-- | Functions given to the React.js components, to interact with the async devices.
-- | They're the same functions as `Plumbing.Logic`, but with parameters applied.
type LogicFunctions
  = { onImport :: Effect Unit
    , onExport :: Effect Unit
    , onClickedExport :: Effect Unit
    , onNew :: Effect Unit
    , onTimeSpaceNameEdit :: Effect Unit
    , onTimeScaleEdit :: Effect Unit
    , onSettingsEdit :: Effect Unit
    , onReadEULA :: Effect Unit
    , onExploreTimeSpaces :: Effect Unit
    , onClickedNewTimeline :: Effect Unit
    , onClickedEditTimeline :: Int -> Effect Unit
    , onClickedDeleteTimeline :: Either Int (Effect Unit) -> Effect Unit
    }

-- | Create the logical functions
logic :: PrimaryQueues -> PrimarySignals -> LogicFunctions
logic { importQueues
, exportQueue
, settingsEditQueues
, timeSpaceNameEditQueues
, timeScaleEditQueues
, snackbarQueue
, eulaQueue
, exploreTimeSpacesQueues
, dangerConfirmQueues
, newOrEditTimelineQueues
} { settingsSignal
, timeSpaceNameSignal
, timeScaleSignal
, zoomSignal
, timeSpaceSelectedSignal
, timelinesSignal
} =
  { onImport:
      onImport
        { importQueues
        , timeSpaceNameSignal: S.readOnly timeSpaceNameSignal
        , settingsSignal
        }
  , onExport: onExport { exportQueue }
  , onClickedExport: onClickedExport { snackbarQueue }
  , onNew:
      onNew
        { dangerConfirmQueues
        , timeScaleSignal: S.writeOnly timeScaleSignal
        , timeSpaceNameSignal: S.writeOnly timeSpaceNameSignal
        }
  , onTimeSpaceNameEdit: onTimeSpaceNameEdit { timeSpaceNameEditQueues, timeSpaceNameSignal }
  , onTimeScaleEdit: onTimeScaleEdit { timeScaleEditQueues, timeScaleSignal }
  , onSettingsEdit: onSettingsEdit { settingsEditQueues, settingsSignal }
  , onReadEULA: onReadEULA { eulaQueue }
  , onExploreTimeSpaces: onExploreTimeSpaces { exploreTimeSpacesQueues, timeSpaceSelectedSignal }
  , onClickedNewTimeline: onClickedNewTimeline { newOrEditTimelineQueues, timelinesSignal }
  , onClickedEditTimeline: onClickedEditTimeline { newOrEditTimelineQueues, timelinesSignal }
  , onClickedDeleteTimeline: onClickedDeleteTimeline { dangerConfirmQueues, timelinesSignal }
  }
