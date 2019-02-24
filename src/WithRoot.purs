module WithRoot where

import Prelude
import React (ReactElement, ReactClass, pureComponent, createLeafElement)
import MaterialUI.Styles (muiThemeProvider, createMuiTheme)
import MaterialUI.Theme (Theme)
import MaterialUI.CssBaseline (cssBaseline')



theme :: Theme
theme = createMuiTheme {}


withRoot :: ReactElement -> ReactElement
withRoot x =
  let c :: ReactClass {}
      c = pureComponent "WithRoot" \this ->
        pure
          { state: {}
          , render: pure $ muiThemeProvider {theme}
            [ cssBaseline' {}
            , x
            ]
          }
  in  createLeafElement c {}
