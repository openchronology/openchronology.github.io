module Components.Index where

{-|

This is the main entry point React.js component - basically, everything that's in the
user interface visually, lives here.

-}

import Settings (Settings (..))

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
import Effect (Effect)
import Effect.Ref (Ref)
import Queue.Types (allowReading, readOnly) as Q
import Zeta.Types (READ, readOnly) as S
import IxZeta (IxSignal, get) as IxSig
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , toElement, component, setState, getProps, createLeafElement)
import React.DOM (text, div, main)
import React.DOM.Props (style, className) as P
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Drawer (drawer)
import MaterialUI.Enums (title, permanent, right)



-- TODO define which queues, signals, and functions Index will need, and keep separate
-- from others


drawerWidth :: Int
drawerWidth = 240


styles :: _
styles theme =
  { drawer:
    { width: drawerWidth
    , flexShrink: 0
    }
  , drawerPaper:
    { width: drawerWidth
    , paddingTop: theme.spacing.unit * 8
    }
  , content:
    { flexGrow: 1
    , paddingTop: theme.spacing.unit * 8
    , paddingBottom: theme.spacing.unit * 8
    , padding: theme.spacing.unit * 2
    , marginLeft: drawerWidth
    , marginRight: drawerWidth
    , height: "100%"
    , zIndex: theme.zIndex.drawer
    }
  }



type State =
  { isEditable :: Boolean
  }

initialState :: IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState settingsSignal = do
  Settings {isEditable} <- IxSig.get settingsSignal
  pure {isEditable}



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
    c = withStyles styles c'
      where
        c' :: ReactClass
              { classes ::
                { drawer :: String
                , drawerPaper :: String
                , content :: String
                }
              }
        c' = component "Index" constructor'

    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handleChangeEdit this (Settings {isEditable}) = setState this {isEditable}
      in  whileMountedIx settingsSignal "Index" handleChangeEdit constructor
      where
        constructor this = do
          state <- initialState (S.readOnly settingsSignal)
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
              props <- getProps this
              pure $ toElement
                [ topBar
                  { onImport
                  , onExport
                  , onNew
                  , onTimelineNameEdit
                  , onSettingsEdit
                  , timelineNameSignal: S.readOnly timelineNameSignal
                  , settingsSignal: S.readOnly settingsSignal
                  }

                , drawer
                  { className: props.classes.drawer
                  , variant: permanent
                  , classes: {paper: props.classes.drawerPaper}
                  }
                  [ typography {variant: title} [text "Drawer!"]
                  ]

                , main [P.className props.classes.content] -- [P.style {height: "100%", padding: "3em 0"}, ]
                  [ typography {gutterBottom: true, variant: title} [text "Just a Test - blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"]
                  ]

                , drawer
                  { className: props.classes.drawer
                  , variant: permanent
                  , classes: {paper: props.classes.drawerPaper}
                  , anchor: right
                  }
                  [ typography {variant: title} [text "Drawer!"]
                  ]

                , bottomBar
                  { onTimeScaleEdit
                  , zoomSignal
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
