module WithRoot where

{-|

This module sets global styles for Material-UI, and does some linking
with the React.js components to make it all work as intended by the Material-UI
authors.

-}

import Prelude
import React (ReactElement, ReactClass, pureComponent, createLeafElement)
import MaterialUI.Styles (muiThemeProvider, createMuiTheme)
import MaterialUI.Theme (Theme)
import MaterialUI.CssBaseline (cssBaseline')



-- | The primary and secondary colors
theme :: Theme
theme = createMuiTheme
  { palette:
    { primary:
      { main: "#7b1fa2"
      }
    , secondary:
      { main: "#e91e63"
      }
    }
  }



-- | Wrap the content element with the theme provider
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
