module Components.AppBar where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React (ReactElement, ReactClass, pureComponent, getProps, createLeafElement)
import React.DOM (text)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, static, inherit)



styles :: _
styles =
  { root:
    { flexGrow: 1
    }
  , grow:
    { flexGrow: 1
    }
  }


indexAppBar :: {onImport :: Effect Unit, onExport :: Effect Unit} -> ReactElement
indexAppBar {onImport, onExport} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles (const styles) c
    c :: ReactClass {classes :: {root :: String, grow :: String}}
    c = pureComponent "IndexAppBar" \this ->
      pure
        { state: {}
        , render: do
            props <- getProps this
            pure $ appBar {position: static, className: props.classes.root}
              [ toolbar_
                [ typography {variant: title, color: inherit, className: props.classes.grow} [text "OpenChronology"]
                , button {color: inherit, onClick: mkEffectFn1 (const onImport)} [text "Import"]
                , button {color: inherit, onClick: mkEffectFn1 (const onExport)} [text "Export"]
                ]
              ]
        }
