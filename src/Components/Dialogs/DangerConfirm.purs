module Components.Dialogs.DangerConfirm where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Timer (setTimeout)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , createLeafElement, component, setState, getState, getProps
  )
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Colors (red)
import MaterialUI.Button (button)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Styles (withStyles)
import MaterialUI.Theme (Theme)
import IOQueues (IOQueues (..))
import Queue.One (Queue, put)


newtype DangerConfirm = DangerConfirm
  { content :: Array ReactElement
  , submit :: String
  }


type State =
  { open :: Boolean
  , content :: Array ReactElement
  , submit :: String
  }


initialState :: State
initialState =
  { open: false
  , content: []
  , submit: "Submit"
  }


styles :: Theme -> _
styles theme =
  { submit:
    { color: theme.palette.getContrastText red."500"
    , backgroundColor: red."500"
    , "&:hover":
      { backgroundColor: red."700"
      }
    }
  }


dangerConfirmDialog :: { dangerConfirmQueues :: IOQueues Queue DangerConfirm Boolean
                       } -> ReactElement
dangerConfirmDialog {dangerConfirmQueues: IOQueues {input, output}} = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        c' :: ReactClass {classes :: {submit :: String}}
        c' = component "DangerConfirmDialog" constructor'

    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handlerOpen :: _ -> DangerConfirm -> Effect Unit
          handlerOpen this (DangerConfirm {content, submit}) = do
            _ <- setTimeout 250 (setState this {open: true})
            setState this {content, submit}
      in  whileMountedOne input handlerOpen constructor
      where
        constructor this =
          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initialState
            , render: do
              let close = do
                    setState this {open: false}
                    put output false
                  submit' = do
                    setState this {open: false}
                    put output true

              {open,content,submit} <- getState this
              props <- getProps this
              pure $ dialog''
                { onClose: mkEffectFn1 (const close)
                , open
                , "aria-labelledby": "dangerConfirm-dialog-title"
                }
                [ dialogTitle { id: "dangerConfirm-dialog-title" } [text "Confirm"]
                , dialogContent_ content
                , dialogActions_
                  [ button {onClick: mkEffectFn1 (const close)} [text "Cancel"]
                  , button
                    { onClick: mkEffectFn1 (const submit')
                    , className: props.classes.submit
                    , autoFocus: true
                    } [text submit]
                  ]
                ]
            }
