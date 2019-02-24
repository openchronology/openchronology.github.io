module Components.AppBar where

import Prelude
import React (ReactElement, ReactClass, pureComponent, getProps, createLeafElement)
import React.DOM (text)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Button (button)
import MaterialUI.Input (input')
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


indexAppBar :: ReactElement
indexAppBar = createLeafElement (withStyles (const styles) c) {}
  where
    c :: ReactClass {classes :: {root :: String, grow :: String}}
    c = pureComponent "IndexAppBar" \this ->
      pure
        { state: {}
        , render: do
            props <- getProps this
            pure $ appBar {position: static, className: props.classes.root}
              [ toolbar_
                [ typography {variant: title, color: inherit, className: props.classes.grow} [text "OpenChronology"]
                , button {color: inherit} [text "Import"] -- input' {type: "file", inputProps: {accept: ".och"}}
                , button {color: inherit} [text "Export"]
                ]
              ]
        }
