module Components.Dialogs.New (newDialog) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues(..))
import React
  (ReactElement, ReactClass, ReactClassConstructor, component, setState, getState, getProps, createLeafElement)
import React.DOM (text, strong, span)
import React.DOM.Props (dangerouslySetInnerHTML) as RP
import React.DOM.NonBlockingSpace (nbsp)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Theme (Theme)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Styles (withStyles)
import MaterialUI.Button (button)
import MaterialUI.Enums (primary, secondary, subheading)
import MaterialUI.Typography (typography)

type State
  = { open :: Boolean }

initialState :: State
initialState = { open: false }

styles :: Theme -> _
styles theme =
  { buttons:
      { zIndex: 2
      }
  }

newDialog ::
  { newQueues :: IOQueues Queue Unit Boolean
  } ->
  ReactElement
newDialog { newQueues: IOQueues { input, output } } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' :: ReactClass { classes :: { buttons :: String } }
    c' = component "New" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }
    in
      whileMountedOne input handlerOpen constructor
    where
    constructor this =
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state: initialState
        , render:
            do
              let
                close = do
                  setState this initialState
                  put output false

                submit = do
                  put output true
                  setState this initialState
              { open } <- getState this
              props <- getProps this
              pure
                $ dialog'' { onClose: mkEffectFn1 (const close), open, "aria-labelledby": "new-dialog-title" }
                    [ dialogTitle { id: "new-dialog-title" } [ text "New Timeline" ]
                    , dialogContent_
                        [ typography
                            { gutterBottom: true
                            , variant: subheading
                            }
                            [ strong [] [ text "Warning!" ]
                            , nbsp
                            , text "Creating a new timeline will erase all content! You can keep any unsaved work with the \"Export\" button."
                            ]
                        ]
                    , dialogActions { className: props.classes.buttons }
                        [ button { onClick: mkEffectFn1 (const close), color: primary } [ text "Cancel" ]
                        , button
                            { onClick: mkEffectFn1 (const submit)
                            , color: secondary
                            , autoFocus: true
                            }
                            [ text "Create New Timeline" ]
                        ]
                    ]
        }
