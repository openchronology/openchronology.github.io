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

module Components.Dialogs.EULA where

import Prelude
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Web.Storage.Storage (getItem, setItem)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , createLeafElement, component, setState, getState)
import React.DOM (text, a, strong)
import React.DOM.Props (href)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (body2, subheading, secondary, headline)




eulaLocalStorageKey :: String
eulaLocalStorageKey = "eula"


type State = {open :: Boolean}

initialState :: Effect State
initialState = do
  store <- window >>= localStorage
  mExists <- getItem eulaLocalStorageKey store
  pure {open: not (isJust mExists)}


eulaDialog :: ReactElement
eulaDialog = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "EULADialog" constructor'

    constructor' :: ReactClassConstructor _ State _
    constructor' this = do
      state <- initialState
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render: do
          let handleAccept = do
                store <- window >>= localStorage
                setItem eulaLocalStorageKey "accepted" store
                setState this {open: false}
          {open} <- getState this
          pure $
            dialog''
              { disableBackdropClick: true
              , disableEscapeKeyDown: true
              , open
              , "aria-labelledby": "eula-dialog-title"
              }
              [ dialogTitle {id: "eula-dialog-title"} [text "End User License Agreement"]
              , dialogContent_ eulaText
              , dialogActions_
                [ button {onClick: mkEffectFn1 (const handleAccept), color: secondary}
                  [text "I Accept"]
                ]
              ]
        }


eulaText :: Array ReactElement
eulaText =
  [ typography {variant: headline, gutterBottom: true} [text "Copyright Disclaimer"]
  , typography {variant: subheading}
    [ strong [] [text "OpenChronology"]
    , text " - an application for graphing and visualizing timelines."
    ]
  , typography {variant: subheading, paragraph: true}
    [ text "Copyright (C) 2020  Athan Clark"
    ]
  , typography {variant: body2, paragraph: true}
    [ text """
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.
"""
    ]
  , typography {variant: body2, paragraph: true}
    [ text """
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.
"""
    ]
  , typography {variant: body2}
    [ text """
You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see 
"""
    , a [href "https://www.gnu.org/licenses/"] [text "https://www.gnu.org/licenses/"]
    , text "."
    ]
  ]
