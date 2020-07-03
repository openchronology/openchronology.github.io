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
module Components.Dialogs.Welcome.Copyright where

import Prelude
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Web.Storage.Storage (getItem, setItem, removeItem)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , setState
  , getState
  )
import React.DOM (text, a, strong)
import React.DOM.Props (href)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (body2, subtitle1, secondary, h5)
import IOQueues (IOQueues(..))
import Queue.One (Queue, put) as Q

copyrightLocalStorageKey :: String
copyrightLocalStorageKey = "copyright"

type State
  = { open :: Boolean }

initialState :: State
initialState = { open: false } -- Opened by Welcome dialog

copyrightDialog ::
  { copyrightQueues :: IOQueues Q.Queue Unit Boolean
  } ->
  ReactElement
copyrightDialog { copyrightQueues: IOQueues { input, output } } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "CopyrightDialog" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' = whileMountedOne input (\this _ -> setState this { open: true }) constructor
    where
    constructor this = do
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state: initialState
        , render:
            do
              let
                handleAccept = do
                  store <- window >>= localStorage
                  setItem copyrightLocalStorageKey "accepted" store
                  setState this { open: false }
                  Q.put output true

                handleNotAccepted = do
                  store <- window >>= localStorage
                  removeItem copyrightLocalStorageKey store
                  setState this { open: false }
                  Q.put output false
              { open } <- getState this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const handleNotAccepted)
                    , open
                    , "aria-labelledby": "copyright-dialog-title"
                    }
                    [ dialogTitle { id: "copyright-dialog-title" } [ text "Copyright Disclaimer" ]
                    , dialogContent_ copyrightText
                    , dialogActions_
                        [ button { onClick: mkEffectFn1 (const handleNotAccepted) }
                            [ text "I Don't Accept" ]
                        , button { onClick: mkEffectFn1 (const handleAccept), color: secondary }
                            [ text "I Accept" ]
                        ]
                    ]
        }

copyrightText :: Array ReactElement
copyrightText =
  [ typography { variant: h5, gutterBottom: true } [ text "Copyright Disclaimer" ]
  , typography { variant: subtitle1 }
      [ strong [] [ text "OpenChronology" ]
      , text " - an application for graphing and visualizing timelines."
      ]
  , typography { variant: subtitle1, paragraph: true }
      [ text "Copyright (C) 2020  Athan Clark"
      ]
  , typography { variant: body2, paragraph: true }
      [ text
          """
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.
"""
      ]
  , typography { variant: body2, paragraph: true }
      [ text
          """
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.
"""
      ]
  , typography { variant: body2 }
      [ text
          """
You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see 
"""
      , a [ href "https://www.gnu.org/licenses/" ] [ text "https://www.gnu.org/licenses/" ]
      , text "."
      ]
  ]
