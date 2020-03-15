module Components.BottomBar (bottomBar) where

import Prelude hiding (div)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React (ReactElement, ReactClass, pureComponent, getProps, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (subheading, absolute, inherit, dense)



styles :: _
styles theme =
  { root:
    { flexGrow: 1
    , top: "auto"
    , bottom: 0
    }
  , center:
    { flexGrow: 1
    , textAlign: "center"
    }
  }


bottomBar :: { onTimeScaleEdit :: Effect Unit
             } -> ReactElement
bottomBar {onTimeScaleEdit} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
    c :: ReactClass {classes :: {root :: String, center :: String}}
    c = pureComponent "BottomBar" \this ->
      pure
        { state: {}
        , render: do
            props <- getProps this
            pure $ appBar {position: absolute, className: props.classes.root}
              [ toolbar {variant: dense}
                [ typography {variant: subheading, color: inherit} [text "Zoom"]
                , typography {variant: subheading, color: inherit} [text "1:1px"]
                , div [RP.className props.classes.center] []
                , button {color: inherit, onClick: mkEffectFn1 (const onTimeScaleEdit)} [text "TimeScale (Years)"]
                ]
              ]
        }
