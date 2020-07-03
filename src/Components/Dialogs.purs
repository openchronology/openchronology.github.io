module Components.Dialogs where

import Plumbing (PrimaryQueues, PrimarySignals, LogicFunctions)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import Components.Dialogs.TimeSpaceNameEdit (timeSpaceNameEditDialog)
import Components.Dialogs.TimeScaleEdit (timeScaleEditDialog)
import Components.Dialogs.SettingsEdit (settingsEditDialog)
import Components.Dialogs.Welcome (welcomeDialog)
import Components.Dialogs.Welcome.EULA (eulaDialog)
import Components.Dialogs.Welcome.Copyright (copyrightDialog)
import Components.Dialogs.Welcome.Cookie (cookieDialog)
import Components.Dialogs.ExploreTimeSpaces (exploreTimeSpacesDialog)
import Components.Dialogs.DangerConfirm (dangerConfirmDialog)
import Components.Dialogs.NewOrEditTimeline (newOrEditTimelineDialog)
import Components.Dialogs.NewOrEditEventOrTimeSpan (newOrEditEventOrTimeSpanDialog)
import React (ReactElement)
import Queue.Types (allowReading, readOnly) as Q
import Zeta.Types (readOnly) as S

dialogs ::
  { primaryQueues :: PrimaryQueues
  , primarySignals :: PrimarySignals
  , logicFunctions :: LogicFunctions
  } ->
  Array ReactElement
dialogs { primaryQueues:
    { importQueues
    , exportQueue
    , timeSpaceNameEditQueues
    , timeScaleEditQueues
    , settingsEditQueues
    , newOrEditTimelineQueues
    , newOrEditEventOrTimeSpanQueues
    , welcomeQueue
    , eulaQueues
    , copyrightQueues
    , cookieQueues
    , exploreTimeSpacesQueues
    , dangerConfirmQueues
    }
, primarySignals:
    { timeSpaceNameSignal
    , settingsSignal
    , timeScaleSignal
    , exploreTimeSpacesSignal
    , timeSpaceSelectedSignal
    }
, logicFunctions:
    { onClickedExport
    , onNew
    , onReadEULA
    }
} =
  [ importDialog importQueues
  , exportDialog
      { exportQueue: Q.readOnly (Q.allowReading exportQueue)
      , onClickedExport
      }
  , timeSpaceNameEditDialog
      { timeSpaceNameSignal: S.readOnly timeSpaceNameSignal
      , settingsSignal: S.readOnly settingsSignal
      , timeSpaceNameEditQueues
      }
  , timeScaleEditDialog
      { timeScaleSignal: S.readOnly timeScaleSignal
      , settingsSignal: S.readOnly settingsSignal
      , timeScaleEditQueues
      }
  , settingsEditDialog
      { settingsSignal: S.readOnly settingsSignal
      , settingsEditQueues
      , onNew
      , onReadEULA
      }
  , newOrEditTimelineDialog
      { newOrEditTimelineQueues
      , settingsSignal: S.readOnly settingsSignal
      }
  , newOrEditEventOrTimeSpanDialog
      { newOrEditEventOrTimeSpanQueues
      , settingsSignal: S.readOnly settingsSignal
      , timeScaleSignal: S.readOnly timeScaleSignal
      }
  , welcomeDialog
      { welcomeQueue: Q.readOnly (Q.allowReading welcomeQueue)
      , eulaQueues
      , copyrightQueues
      , cookieQueues
      }
  , eulaDialog { eulaQueues }
  , copyrightDialog { copyrightQueues }
  , cookieDialog { cookieQueues }
  , exploreTimeSpacesDialog
      { exploreTimeSpacesSignal: S.readOnly exploreTimeSpacesSignal
      , timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
      , exploreTimeSpacesQueues
      }
  , dangerConfirmDialog { dangerConfirmQueues }
  ]
