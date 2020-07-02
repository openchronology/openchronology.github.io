module Components.Dialogs.Welcome where

import Components.Dialogs.Welcome.EULA (eulaLocalStorageKey)
import Prelude
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Web.Storage.Storage (getItem, setItem)
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
import Queue.One (READ, Queue) as Q

welcomeLocalStorageKey :: String
welcomeLocalStorageKey = "welcome"

type State
  = { open :: Boolean
    , copyrightAgreed :: Boolean
    , eulaAgreed :: Boolean
    , cookieAgreed :: Boolean
    }

initialState :: Effect State
initialState = do
  store <- window >>= localStorage
  mExists <- getItem welcomeLocalStorageKey store
  mEulaAgreed <- getItem eulaLocalStorageKey store
  pure
    { open: not (isJust mExists)
    , eulaAgreed: not (isJust mEulaAgreed)
    , cookieAgreed: false -- FIXME
    , copyrightAgreed: false -- FIXME
    }

welcomeDialog ::
  { welcomeQueue :: Q.Queue ( read :: Q.READ ) Unit -- ^ Write to this to open the dialog
  } ->
  ReactElement
welcomeDialog { welcomeQueue } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "WelcomeDialog" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' = whileMountedOne welcomeQueue (\this _ -> setState this { open: true }) constructor
    where
    constructor this = do
      state <- initialState
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                handleNext = do
                  store <- window >>= localStorage
                  setItem welcomeLocalStorageKey "read" store
                  setState this { open: false } -- FIXME should reveal left the "tutorial" option
              { open, eulaAgreed, cookieAgreed, copyrightAgreed } <- getState this
              pure
                $ dialog''
                    { disableBackdropClick: true
                    , disableEscapeKeyDown: true
                    , open
                    , "aria-labelledby": "welcome-dialog-title"
                    }
                    [ dialogTitle { id: "welcome-dialog-title" } [ text "Welcome!" ]
                    , dialogContent_ welcomeText
                    , dialogActions_
                        [ button
                            { onClick: mkEffectFn1 (const handleNext)
                            , color: secondary
                            , disabled: not (eulaAgreed && cookieAgreed && copyrightAgreed)
                            }
                            [ text "Next" ]
                        ]
                    ]
        }

welcomeText :: Array ReactElement
welcomeText =
  [ typography { variant: h5, gutterBottom: true } [ text "Welcome to OpenChronology" ]
  , typography { variant: body2, paragraph: true }
      [ text
          """
Before we get started, we have a few copyright agreements and cookie policies we'll
need you to read and agree to.
"""
      ]
  ]
