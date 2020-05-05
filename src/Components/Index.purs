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
  , toElement, component, setState, getState, getProps, createLeafElement)
import React.DOM (text, div, main)
import React.DOM.Props (style, className) as P
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Drawer (drawer)
import MaterialUI.Divider (divider_)
import MaterialUI.List (list_, list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText')
import MaterialUI.Enums (title, subheading, permanent, right)
import Unsafe.Coerce (unsafeCoerce)



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
    , marginTop: theme.spacing.unit * 6.0
    , marginBottom: theme.spacing.unit * 6.0
    , height: "calc(100vh - " <> show (theme.spacing.unit * 12.0) <> "px)"
    }
  , contentEditMode:
    { flexGrow: 1
    , paddingTop: theme.spacing.unit * 6.0
    , marginTop: 0
    , marginBottom: theme.spacing.unit * 6.0
    , marginLeft: drawerWidth
    , marginRight: drawerWidth
    , height: "calc(100vh - " <> show (theme.spacing.unit * 12.0) <> "px)"
    }
  , content:
    { flexGrow: 1
    , paddingTop: theme.spacing.unit * 6.0
    , marginTop: 0
    , marginBottom: theme.spacing.unit * 6.0
    , height: "calc(100vh - " <> show (theme.spacing.unit * 12.0) <> "px)"
    }
  , rightDrawerList:
    { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 2.0)) <> "px)"
    , overflowY: "auto"
    }
  , leftDrawerList:
    { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 3.0)) <> "px)"
    , overflowY: "auto"
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
                , contentEditMode :: String
                , rightDrawerList :: String
                , leftDrawerList :: String
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
              {isEditable} <- getState this
              let leftDrawer
                    | isEditable =
                        [ drawer
                          { className: props.classes.drawer
                          , variant: permanent
                          , classes: {paper: props.classes.drawerPaper}
                          }
                          [ typography {variant: title} [text "Timelines"]
                          , list {className: props.classes.leftDrawerList} $ map mkTextItem
                              [ "Timeline A"
                              , "Timeline B"
                              , "Timeline C"
                              , "Timeline D"
                              , "Timeline E"
                              , "Timeline F"
                              , "Timeline G"
                              ]
                          , divider_ []
                          , typography {variant: title} [text "Events and TimeSpans"]
                          , typography {variant: subheading} [text "For Multiple Timelines"]
                          , list {className: props.classes.leftDrawerList} $ map mkTextItemTime
                            [ {name: "Event A", time: "20200130"}
                            , {name: "Event B", time: "20200131"}
                            , {name: "TimeSpan C", time: "20200202"}
                            , {name: "TimeSpan D", time: "20200204"}
                            , {name: "Event E", time: "20200208"}
                            , {name: "Event F", time: "20200211"}
                            ]
                          ]
                        ]
                    | otherwise = []
                  rightDrawer
                    | isEditable =
                        [ drawer
                          { className: props.classes.drawer
                          , variant: permanent
                          , classes: {paper: props.classes.drawerPaper}
                          , anchor: right
                          }
                          [ typography {variant: title} [text "Events and TimeSpans"]
                          , typography {variant: subheading} [text "For Timeline \"A\""]
                          , list {className: props.classes.rightDrawerList} $ map mkTextItemTime
                            [ {name: "Event A", time: "20200130"}
                            , {name: "Event B", time: "20200131"}
                            , {name: "TimeSpan C", time: "20200202"}
                            , {name: "TimeSpan D", time: "20200204"}
                            , {name: "Event E", time: "20200208"}
                            , {name: "Event F", time: "20200211"}
                            ]
                          ]
                        ]
                    | otherwise = []
              pure $ toElement $
                [ topBar
                  { onImport
                  , onExport
                  , onNew
                  , onTimelineNameEdit
                  , onSettingsEdit
                  , timelineNameSignal: S.readOnly timelineNameSignal
                  , settingsSignal: S.readOnly settingsSignal
                  }
                ] <> leftDrawer <>

                [ main
                  [ P.className $
                    if isEditable
                      then props.classes.contentEditMode
                      else props.classes.content
                  ]
                  [ typography {gutterBottom: true, variant: title}
                    [text "Just a Test - blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"]
                  ]
                ] <> rightDrawer <>

                [ bottomBar
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

    mkTextItem t = listItem {button: true} [listItemText' {primary: t}]
    mkTextItemTime {name,time} = listItem {button: true} [listItemText' {primary: name, secondary: time}]
