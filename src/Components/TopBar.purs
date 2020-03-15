module Components.TopBar (topBar) where

import Timeline.Data.TimelineName (TimelineName)

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
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)



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
  }

initialState :: IxSig.IxSignal (read :: S.READ) TimelineName -> Effect State
initialState timelineNameSignal = do
  {title} <- IxSig.get timelineNameSignal
  pure {title}


topBar :: { onImport :: Effect Unit
          , onExport :: Effect Unit
          , onTimelineNameEdit :: Effect Unit
          , timelineNameSignal :: IxSig.IxSignal (read :: S.READ) TimelineName
          } -> ReactElement
topBar {onImport, onExport, onTimelineNameEdit, timelineNameSignal} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
      where
        c :: ReactClass {classes :: {root :: String, center :: String}}
        c = component "TopBar" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      whileMountedIx timelineNameSignal "TopBar" (\this {title} -> setState this {title})
      constructor
      where
        constructor this = do
          state <- initialState timelineNameSignal
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
                props <- getProps this
                {title: titleValue} <- getState this
                pure $ appBar {position: absolute, className: props.classes.root}
                  [ toolbar {variant: dense}
                    [ typography {variant: title, color: inherit} [text "OpenChronology"]
                    , div [RP.className props.classes.center]
                      [ button {color: inherit, onClick: mkEffectFn1 (const onTimelineNameEdit)} [text titleValue]
                      ]
                    , button {color: inherit, onClick: mkEffectFn1 (const onImport)} [text "Import"]
                    , button {color: inherit, onClick: mkEffectFn1 (const onExport)} [text "Export"]
                    , iconButton {color: inherit} [settingsIcon]
                    , let props' :: {href :: String}
                          props' = unsafeCoerce
                            { color: inherit
                            , href: "https://github.com/openchronology/openchronology.github.io/"
                            , target: "__blank"
                            }
                      in  iconButton props'
                      [icon' {className: "fab fa-github"}]
                    ]
                  ]
            }
