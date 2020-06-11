module Components.Index where

{-|

This is the main entry point React.js component - basically, everything that's in the
user interface visually, lives here.

-}
import Settings (Settings(..))
import Components.TopBar (topBar)
import Components.BottomBar (bottomBar)
import Components.Drawers.Timelines (timelinesDrawer)
import Components.Drawers.Siblings (siblingsDrawer)
import Components.Drawers.Children (childrenDrawer)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import Components.Dialogs.TimeSpaceNameEdit (timeSpaceNameEditDialog)
import Components.Dialogs.TimeScaleEdit (timeScaleEditDialog)
import Components.Dialogs.SettingsEdit (settingsEditDialog)
import Components.Dialogs.EULA (eulaDialog)
import Components.Dialogs.ExploreTimeSpaces (exploreTimeSpacesDialog)
import Components.Dialogs.DangerConfirm (dangerConfirmDialog)
import Components.Dialogs.NewOrEditTimeline (newOrEditTimelineDialog)
import Components.Snackbar (snackbars)
import WithRoot (withRoot)
import Plumbing (PrimaryQueues, PrimarySignals, LogicFunctions)
import Prelude hiding (div)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Queue.Types (allowReading, readOnly) as Q
import Zeta.Types (READ, readOnly) as S
import IxZeta (IxSignal, get) as IxSig
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , toElement
  , component
  , setState
  , getState
  , getProps
  , createLeafElement
  )
import React.DOM (text, main)
import React.DOM.Props (className) as P
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Drawer (drawer)
import MaterialUI.Divider (divider_)
import MaterialUI.Enums (title, permanent, right)
import MaterialUI.Theme (Theme)

-- TODO define which queues, signals, and functions Index will need, and keep separate
-- from others
drawerWidth :: Int
drawerWidth = 240

styles :: Theme -> _
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
  , contentSearchMode:
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
  }

type State
  = { isSearchable :: Boolean
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState settingsSignal = do
  Settings { isSearchable } <- IxSig.get settingsSignal
  pure { isSearchable }

-- | This component takes the top-level functions, queues, and signals created
-- | in `Main`, and distributes them to its child elements.
index ::
  { stateRef :: Ref Unit -- FIXME change to whatever state is changed - is this even needed?
  , primaryQueues :: PrimaryQueues
  , primarySignals :: PrimarySignals
  , logicFunctions :: LogicFunctions
  } ->
  ReactElement
index { stateRef
, primaryQueues:
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
, primarySignals:
    { settingsSignal
    , timeSpaceNameSignal
    , timeScaleSignal
    , zoomSignal
    , exploreTimeSpacesSignal
    , timeSpaceSelectedSignal
    , timelinesSignal
    }
, logicFunctions:
    { onImport
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
    }
} = withRoot e
  where
  e = createLeafElement c {}

  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' ::
      ReactClass
        { classes ::
            { drawer :: String
            , drawerPaper :: String
            , content :: String
            , contentSearchMode :: String
            }
        }
    c' = component "Index" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handleChangeEdit this (Settings { isSearchable }) = setState this { isSearchable }
    in
      whileMountedIx settingsSignal "Index" handleChangeEdit constructor
    where
    constructor this = do
      state <- initialState (S.readOnly settingsSignal)
      pure
        { state
        , componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , render:
            do
              props <- getProps this
              { isSearchable } <- getState this
              let
                leftDrawer
                  | isSearchable =
                    [ drawer
                        { className: props.classes.drawer
                        , variant: permanent
                        , classes: { paper: props.classes.drawerPaper }
                        }
                        [ timelinesDrawer
                            { settingsSignal: S.readOnly settingsSignal
                            , timelinesSignal: S.readOnly timelinesSignal
                            , onClickedNewTimeline
                            , onClickedEditTimeline
                            , onClickedDeleteTimeline: onClickedDeleteTimeline <<< Left
                            }
                        , divider_ []
                        , siblingsDrawer { settingsSignal: S.readOnly settingsSignal }
                        ]
                    ]
                  | otherwise = []

                rightDrawer
                  | isSearchable =
                    [ drawer
                        { className: props.classes.drawer
                        , variant: permanent
                        , classes: { paper: props.classes.drawerPaper }
                        , anchor: right
                        }
                        [ childrenDrawer { settingsSignal: S.readOnly settingsSignal }
                        ]
                    ]
                  | otherwise = []
              pure $ toElement
                $ [ topBar
                      { onImport
                      , onExport
                      , onTimeSpaceNameEdit
                      , onSettingsEdit
                      , timeSpaceNameSignal: S.readOnly timeSpaceNameSignal
                      , settingsSignal: S.readOnly settingsSignal
                      , onExploreTimeSpaces
                      }
                  ]
                <> leftDrawer
                <> [ main
                      [ P.className
                          $ if isSearchable then
                              props.classes.contentSearchMode
                            else
                              props.classes.content
                      ]
                      [ typography { gutterBottom: true, variant: title }
                          [ text "Just a Test - blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah" ]
                      ]
                  ]
                <> rightDrawer
                <> [ bottomBar
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
                      { onDelete: onClickedDeleteTimeline <<< Right
                      , newOrEditTimelineQueues
                      , settingsSignal: S.readOnly settingsSignal
                      }
                  , eulaDialog { eulaQueue: Q.readOnly (Q.allowReading eulaQueue) }
                  , exploreTimeSpacesDialog
                      { exploreTimeSpacesSignal: S.readOnly exploreTimeSpacesSignal
                      , timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
                      , exploreTimeSpacesQueues
                      }
                  , dangerConfirmDialog { dangerConfirmQueues }
                  , snackbars (Q.readOnly (Q.allowReading snackbarQueue))
                  ]
        }
