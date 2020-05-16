{-
OpenChronology - an application for graphing and visualizing timelines.
Copyright (C) 2020  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

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
