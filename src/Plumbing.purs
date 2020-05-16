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
  ( onImport, onExport, onClickedExport
  , onNew, onTimelineNameEdit, onTimeScaleEdit, onSettingsEdit
  )

import Components.Dialogs.Import (ImportDialog) as Import
import Components.Dialogs.Export (ExportDialog) as Export
import Timeline.Data.TimelineName (TimelineName, newTimelineNameSignal)
import Timeline.Data.TimeScale (TimeScale, newTimeScaleSignal)
import Components.Snackbar (SnackbarContent)
import Settings (Settings, newSettingsSignal)

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Queue.One (Queue, new) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new) as IOQueues
import Zeta.Types (WRITE, READ, readOnly, writeOnly) as S
import IxZeta (IxSignal, make) as IxSig
import Web.File.File (File)


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
-- | They're the same functions as `Plumbing.Logic`, but with parameters applied.
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
  { onImport: onImport
    { importQueues
    , timelineNameSignal: S.readOnly timelineNameSignal
    , settingsSignal
    }
  , onExport: onExport {exportQueue}
  , onClickedExport: onClickedExport {snackbarQueue}
  , onNew: onNew
    { newQueues
    , timeScaleSignal: S.writeOnly timeScaleSignal
    , timelineNameSignal: S.writeOnly timelineNameSignal
    }
  , onTimelineNameEdit: onTimelineNameEdit {timelineNameEditQueues,timelineNameSignal}
  , onTimeScaleEdit: onTimeScaleEdit {timeScaleEditQueues,timeScaleSignal}
  , onSettingsEdit: onSettingsEdit {settingsEditQueues,settingsSignal}
  }
