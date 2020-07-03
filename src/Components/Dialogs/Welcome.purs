module Components.Dialogs.Welcome where

import Components.Dialogs.Welcome.EULA (eulaLocalStorageKey)
import Components.Dialogs.Welcome.Copyright (copyrightLocalStorageKey)
import Components.Dialogs.Welcome.Cookie (cookieLocalStorageKey)
import Prelude
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
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
import React.DOM (text, a, strong, br)
import React.DOM.Props (href)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Checkbox (checkbox')
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (body2, subtitle1, secondary, h5, contained)
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import Queue.One (READ, Queue) as Q

welcomeLocalStorageKey :: String
welcomeLocalStorageKey = "welcome"

type State
  = { open :: Boolean
    , tutorialInquireOpen :: Boolean
    , copyrightAgreed :: Boolean
    , eulaAgreed :: Boolean
    , cookieAgreed :: Boolean
    }

initialState :: Effect State
initialState = do
  store <- window >>= localStorage
  mExists <- getItem welcomeLocalStorageKey store
  mEulaAgreed <- getItem eulaLocalStorageKey store
  mCopyrightAgreed <- getItem copyrightLocalStorageKey store
  mCookieAgreed <- getItem cookieLocalStorageKey store
  pure
    { open: not (isJust mExists)
    , tutorialInquireOpen: false
    , eulaAgreed: isJust mEulaAgreed
    , copyrightAgreed: isJust mCopyrightAgreed
    , cookieAgreed: isJust mCookieAgreed
    }

welcomeDialog ::
  { welcomeQueue :: Q.Queue ( read :: Q.READ ) Unit -- ^ Write to this to open the dialog
  , eulaQueues :: IOQueues Q.Queue Unit Boolean
  , copyrightQueues :: IOQueues Q.Queue Unit Boolean
  , cookieQueues :: IOQueues Q.Queue Unit Boolean
  } ->
  ReactElement
welcomeDialog { welcomeQueue, eulaQueues, copyrightQueues, cookieQueues } = createLeafElement c {}
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
                  setState this { tutorialInquireOpen: true }

                handleClickEula =
                  launchAff_ do
                    eulaAgreed <- IOQueues.callAsync eulaQueues unit
                    liftEffect (setState this { eulaAgreed })

                handleClickCopyright =
                  launchAff_ do
                    copyrightAgreed <- IOQueues.callAsync copyrightQueues unit
                    liftEffect (setState this { copyrightAgreed })

                handleClickCookie =
                  launchAff_ do
                    cookieAgreed <- IOQueues.callAsync cookieQueues unit
                    liftEffect (setState this { cookieAgreed })

                handleNoTutorial = do
                  _ <- setTimeout 1000 (setState this { tutorialInquireOpen: false })
                  setState this { open: false }

                handleTutorial = do
                  _ <- setTimeout 1000 (setState this { tutorialInquireOpen: false })
                  -- TODO iterate the tutorial procedure
                  setState this { open: false }
              { open, tutorialInquireOpen, eulaAgreed, cookieAgreed, copyrightAgreed } <- getState this
              pure
                $ dialog''
                    { disableBackdropClick: true
                    , disableEscapeKeyDown: true
                    , open
                    , "aria-labelledby": "welcome-dialog-title"
                    }
                    [ dialogTitle { id: "welcome-dialog-title" } [ text "Welcome!" ]
                    , dialogContent_
                        $ if not tutorialInquireOpen then
                            welcomeText
                              <> [ checkbox' { checked: eulaAgreed }
                                , button { onClick: mkEffectFn1 (const handleClickEula), variant: contained } [ text "End User License Agreement" ]
                                , br []
                                , checkbox' { checked: copyrightAgreed }
                                , button { onClick: mkEffectFn1 (const handleClickCopyright), variant: contained } [ text "Copyright Disclaimer" ]
                                , br []
                                , checkbox' { checked: cookieAgreed }
                                , button { onClick: mkEffectFn1 (const handleClickCookie), variant: contained } [ text "Cookie Policy" ]
                                ]
                          else
                            [ typography { variant: body2, paragraph: true }
                                [ text
                                    """
Great! Would you like to view the tutorial, or just jump right in?
"""
                                ]
                            ]
                    , dialogActions_
                        $ if not tutorialInquireOpen then
                            [ button
                                { onClick: mkEffectFn1 (const handleNext)
                                , color: secondary
                                , disabled: not (eulaAgreed && cookieAgreed && copyrightAgreed)
                                }
                                [ text "Next" ]
                            ]
                          else
                            [ button
                                { onClick: mkEffectFn1 (const handleNoTutorial)
                                }
                                [ text "No, Go to the app" ]
                            , button
                                { onClick: mkEffectFn1 (const handleTutorial)
                                , color: secondary
                                }
                                [ text "View the Tutorial" ]
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
