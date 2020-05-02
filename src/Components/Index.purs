module Components.Index where

{-|

This is the main entry point React.js component - basically, everything that's in the
user interface visually, lives here.

-}


import Components.TopBar (topBar)
import Components.BottomBar (bottomBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import Components.Dialogs.New (newDialog)
import Components.Dialogs.TimelineNameEdit (timelineNameEditDialog)
import Components.Dialogs.TimeScaleEdit (timeScaleEditDialog)
import Components.Dialogs.SettingsEdit (settingsEditDialog)
import Components.Snackbar (snackbars)
import WithRoot (withRoot)
import Plumbing (PrimaryQueues, PrimarySignals, LogicFunctions)

import Prelude hiding (div)
import Effect.Ref (Ref)
import Queue.Types (allowReading, readOnly) as Q
import Zeta.Types (readOnly) as S
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (style) as P
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)



-- TODO define which queues, signals, and functions Index will need, and keep separate
-- from others


-- | This component takes the top-level functions, queues, and signals created
-- | in `Main`, and distributes them to its child elements.
index :: { stateRef :: Ref Unit -- FIXME change to whatever state is changed - is this even needed?
         , primaryQueues :: PrimaryQueues
         , primarySignals :: PrimarySignals
         , logicFunctions :: LogicFunctions
         }
      -> ReactElement
index
  { stateRef
  , primaryQueues:
    { importQueues
    , exportQueue
    , newQueues
    , settingsEditQueues
    , timelineNameEditQueues
    , timeScaleEditQueues
    , snackbarQueue
    }
  , primarySignals:
    { settingsSignal
    , timelineNameSignal
    , timeScaleSignal
    , zoomSignal
    }
  , logicFunctions:
    { onImport
    , onExport
    , onClickedExport
    , onNew
    , onTimelineNameEdit
    , onTimeScaleEdit
    , onSettingsEdit
    }
  } = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> pure
          { state: {}
          , render: pure $ toElement
            [ topBar
              { onImport
              , onExport
              , onNew
              , onTimelineNameEdit
              , onSettingsEdit
              , timelineNameSignal: S.readOnly timelineNameSignal
              , settingsSignal: S.readOnly settingsSignal
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
