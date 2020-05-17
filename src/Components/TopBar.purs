module Components.TopBar (topBar) where

{-|

Both TopBar and BottomBar are Material-UI "AppBar"s - giving site-wide functionality, like navigation
and access to control the side-wide settings.

-}
import Timeline.Data.TimelineName (TimelineName(..))
import Settings (Settings(..))
import Prelude hiding (div)
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , component
  , getProps
  , getState
  , setState
  , createLeafElement
  )
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import React.Signal.WhileMounted (whileMountedIx)
import React.SyntheticEvent (currentTarget, NativeEventTarget)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (absolute, inherit, dense)
import MaterialUI.Icons.SettingsIcon (settingsIcon)
import MaterialUI.Menu (menu)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Theme (Theme)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig

-- | This CSS allows the app bar to stretch across the screen
styles :: Theme -> _
styles theme =  -- trace theme \_ ->
  { root:
      { flexGrow: 1
      , zIndex: theme.zIndex.drawer + 1
      , height: theme.spacing.unit * 6.0
      }
  , center:
      { flexGrow: 1
      , textAlign: "center"
      }
  , breadcrumb:
      { overflowX: "auto"
      , whiteSpace: "nowrap"
      }
  , appBarButton:
      { whiteSpace: "nowrap"
      }
  }

type State
  = { title :: String
    , isEditable :: Boolean
    , menuAnchor :: Maybe NativeEventTarget
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) TimelineName ->
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState timelineNameSignal settingsSignal = do
  TimelineName { title } <- IxSig.get timelineNameSignal
  Settings { isEditable } <- IxSig.get settingsSignal
  pure
    { title
    , isEditable
    , menuAnchor: Nothing
    }

-- | The signals give some state to this component, while the functions are
-- | how the component interact with the queues.
topBar ::
  { onImport :: Effect Unit
  , onExport :: Effect Unit
  , onTimelineNameEdit :: Effect Unit
  , onSettingsEdit :: Effect Unit
  , timelineNameSignal :: IxSig.IxSignal ( read :: S.READ ) TimelineName
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  } ->
  ReactElement
topBar { onImport
, onExport
, onTimelineNameEdit: onTimeSpaceNameEdit
, onSettingsEdit
, timelineNameSignal: timeSpaceNameSignal
, settingsSignal
} = createLeafElement c' {}
  where
  c' :: ReactClass {}
  c' = withStyles styles c
    where
    c ::
      ReactClass
        { classes ::
            { root :: String
            , center :: String
            , breadcrumb :: String
            , appBarButton :: String
            }
        }
    c = component "TopBar" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerChangeTitle this (TimelineName { title }) = setState this { title }

      handlerChangeEdit this (Settings { isEditable }) = setState this { isEditable }
    in
      whileMountedIx timeSpaceNameSignal "TopBar" handlerChangeTitle
        $ whileMountedIx settingsSignal "TopBar" handlerChangeEdit constructor
    where
    constructor this = do
      state <- initialState timeSpaceNameSignal settingsSignal
      pure
        { state
        , componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , render:
            do
              props <- getProps this
              { title: titleValue, isEditable, menuAnchor } <- getState this
              pure
                $ appBar { position: absolute, className: props.classes.root }
                    [ toolbar { variant: dense }
                        $ let
                            clickedTimeSpaceButton e
                              | isEditable = do
                                anchor <- currentTarget e
                                setState this { menuAnchor: Just anchor }
                              | otherwise = pure unit -- FIXME open timespace explorer

                            divider :: ReactElement
                            divider = div [ RP.className props.classes.center ] []
                          in
                            [ button
                                { color: inherit
                                , onClick: mkEffectFn1 clickedTimeSpaceButton
                                , title: "TimeSpace Name and Description"
                                , className: props.classes.appBarButton
                                }
                                [ text titleValue ]
                            , divider
                            , button
                                { color: inherit
                                , onClick: mkEffectFn1 (const onImport)
                                , className: props.classes.appBarButton
                                }
                                [ text "Import" ]
                            , button
                                { color: inherit
                                , onClick: mkEffectFn1 (const onExport)
                                , className: props.classes.appBarButton
                                }
                                [ text "Export" ]
                            , iconButton
                                { color: inherit
                                , title: "Settings"
                                , onClick: mkEffectFn1 (const onSettingsEdit)
                                }
                                [ settingsIcon ]
                            ]
                    , let
                        handleCloseMenu = setState this { menuAnchor: Nothing }
                      in
                        menu
                          { id: "topbar-menu"
                          , anchorEl: toNullable menuAnchor
                          , open: isJust menuAnchor
                          , onClose: mkEffectFn1 (const handleCloseMenu)
                          }
                          [ menuItem
                              { onClick:
                                  mkEffectFn1 \_ -> do
                                    handleCloseMenu
                                    onTimeSpaceNameEdit
                              }
                              [ text "Edit" ]
                          , menuItem
                              { onClick: mkEffectFn1 (const handleCloseMenu)
                              }
                              [ text "Explore" ]
                          ]
                    ]
        }
