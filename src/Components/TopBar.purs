module Components.TopBar (topBar) where

{-|

Both TopBar and BottomBar are Material-UI "AppBar"s - giving site-wide functionality, like navigation
and access to control the side-wide settings.

-}


import Timeline.Data.TimelineName (TimelineName (..))
import Settings (Settings (..))

import Prelude hiding (div)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getProps, getState, setState, createLeafElement)
import React.DOM (text, div, img)
import React.DOM.Props (className, src, style) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (absolute, inherit, dense)
import MaterialUI.Icon (icon', icon_)
import MaterialUI.Icons.SettingsIcon (settingsIcon)
import MaterialUI.Icons.AddCircleIcon (addCircleIcon)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig


-- | This CSS allows the app bar to stretch across the screen
styles :: _
styles theme =
  { root:
    { flexGrow: 1
    }
  , center:
    { flexGrow: 1
    , textAlign: "center"
    }
  }


type State =
  { title :: String
  , isEditable :: Boolean
  }

initialState :: IxSig.IxSignal (read :: S.READ) TimelineName
             -> IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState timelineNameSignal settingsSignal = do
  TimelineName {title} <- IxSig.get timelineNameSignal
  Settings {isEditable} <- IxSig.get settingsSignal
  pure {title, isEditable}


-- | The signals give some state to this component, while the functions are
-- | how the component interact with the queues.
topBar :: { onImport :: Effect Unit
          , onExport :: Effect Unit
          , onNew :: Effect Unit
          , onTimelineNameEdit :: Effect Unit
          , onSettingsEdit :: Effect Unit
          , timelineNameSignal :: IxSig.IxSignal (read :: S.READ) TimelineName
          , settingsSignal :: IxSig.IxSignal (read :: S.READ) Settings
          } -> ReactElement
topBar
  { onImport
  , onExport
  , onNew
  , onTimelineNameEdit
  , onSettingsEdit
  , timelineNameSignal
  , settingsSignal
  } = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
      where
        c :: ReactClass {classes :: {root :: String, center :: String}}
        c = component "TopBar" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handlerChangeTitle this (TimelineName {title}) = setState this {title}
          handlerChangeEdit this (Settings {isEditable}) = setState this {isEditable}
      in  whileMountedIx timelineNameSignal "TopBar" handlerChangeTitle $
          whileMountedIx settingsSignal "TopBar" handlerChangeEdit constructor
      where
        constructor this = do
          state <- initialState timelineNameSignal settingsSignal
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
                props <- getProps this
                {title: titleValue, isEditable} <- getState this
                pure $ appBar {position: absolute, className: props.classes.root} $
                  [ toolbar {variant: dense} $
                    let branding :: ReactElement
                        branding = icon_
                          [ img [RP.src "images/logo-white.svg", RP.style {width: "24px", height: "24px"}]
                          ]
                        breadcrumb :: Array ReactElement
                        breadcrumb =
                          [ button
                            { color: inherit
                            , onClick: mkEffectFn1 (const onTimelineNameEdit)
                            , title: "Timeline Name and Description"
                            } [text titleValue]
                          ]
                        divider :: ReactElement
                        divider = div [RP.className props.classes.center] []
                        createEvent :: Array ReactElement
                        createEvent =
                          if isEditable
                            then
                              [ iconButton
                                { color: inherit
                                , title: "Create Event / Time Span"
                                -- , onClick: mkEffectFn1 (const onAddCircleEdit)
                                } [addCircleIcon]
                              ]
                            else []
                        standardButtons :: Array ReactElement
                        standardButtons =
                          [ button {color: inherit, onClick: mkEffectFn1 (const onNew)} [text "New Timeline"]
                          , button {color: inherit, onClick: mkEffectFn1 (const onImport)} [text "Import"]
                          , button {color: inherit, onClick: mkEffectFn1 (const onExport)} [text "Export"]
                          , iconButton
                              { color: inherit
                              , title: "Settings"
                              , onClick: mkEffectFn1 (const onSettingsEdit)
                              } [settingsIcon]
                          ]
                    in  [branding] <> breadcrumb <> [divider] <> createEvent <> standardButtons
                  ]
            }
