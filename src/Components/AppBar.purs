module Components.AppBar (indexAppBar) where

import Prelude hiding (div)
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React (ReactElement, ReactClass, pureComponent, getProps, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, static, inherit, normal)
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


indexAppBar :: { onImport :: Effect Unit
               , onExport :: Effect Unit
               , onNameEdit :: Effect Unit
               } -> ReactElement
indexAppBar {onImport, onExport, onNameEdit} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
    c :: ReactClass {classes :: {root :: String, center :: String}}
    c = pureComponent "IndexAppBar" \this ->
      pure
        { state: {}
        , render: do
            props <- getProps this
            pure $ appBar {position: static, className: props.classes.root}
              [ toolbar_
                [ typography {variant: title, color: inherit} [text "OpenChronology"]
                , div [RP.className props.classes.center]
                  [ button {color: inherit, onClick: mkEffectFn1 (const onNameEdit)} [text "Timeline Name"]
                  ]
                , button {color: inherit, onClick: mkEffectFn1 (const onImport)} [text "Import"]
                , button {color: inherit, onClick: mkEffectFn1 (const onExport)} [text "Export"]
                ]
              ]
        }
