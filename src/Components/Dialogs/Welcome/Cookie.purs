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
module Components.Dialogs.Welcome.Cookie where

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

cookieLocalStorageKey :: String
cookieLocalStorageKey = "cookie"

type State
  = { open :: Boolean }

initialState :: State
initialState = { open: false } -- Opened by Welcome dialog

cookieDialog ::
  { cookieQueues :: IOQueues Q.Queue Unit Boolean
  } ->
  ReactElement
cookieDialog { cookieQueues: IOQueues { input, output } } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "CookieDialog" constructor'

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
                  setItem cookieLocalStorageKey "accepted" store
                  setState this { open: false }
                  Q.put output true

                handleNotAccepted = do
                  store <- window >>= localStorage
                  removeItem cookieLocalStorageKey store
                  setState this { open: false }
                  Q.put output false
              { open } <- getState this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const handleNotAccepted)
                    , open
                    , "aria-labelledby": "cookie-dialog-title"
                    }
                    [ dialogTitle { id: "cookie-dialog-title" } [ text "End User License Agreement" ]
                    , dialogContent_ cookieText
                    , dialogActions_
                        [ button { onClick: mkEffectFn1 (const handleNotAccepted) }
                            [ text "I Don't Accept" ]
                        , button { onClick: mkEffectFn1 (const handleAccept), color: secondary }
                            [ text "I Accept" ]
                        ]
                    ]
        }

cookieText :: Array ReactElement
cookieText =
  [ typography { variant: body2, paragraph: true }
      [ text
          """
<insert cookie policy here>
"""
      ]
  ]
