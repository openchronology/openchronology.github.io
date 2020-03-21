module Components.TopBar (topBar) where

import Timeline.Data.TimelineName (TimelineName (..))
import Settings (Settings (..))

import Prelude hiding (div)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getProps, getState, setState, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, absolute, inherit, dense)
import MaterialUI.Icon (icon')
import MaterialUI.Icons.SettingsIcon (settingsIcon)
import MaterialUI.Icons.GetAppIcon (getAppIcon)
import MaterialUI.Icons.AddCircleIcon (addCircleIcon)
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig



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
                        branding = typography {variant: title, color: inherit} [text "OpenChronology"]
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
                          , iconButton
                              { color: inherit
                              , href: "https://github.com/openchronology/openchronology.github.io/"
                              , target: "__blank"
                              , title: "GitHub"
                              } [icon' {className: "fab fa-github"}]
                          , iconButton
                              { color: inherit
                              , href: "./openchronology-static.zip"
                              , title: "Download App"
                              } [getAppIcon]
                          ]
                    in  [branding] <> breadcrumb <> [divider] <> createEvent <> standardButtons
                  ]
            }
