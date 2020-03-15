module Components.Dialogs.TimeScaleEdit (timeScaleEditDialog) where

import Timeline.Data.TimeScale (TimeScale)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactClass, ReactClassConstructor, ReactElement
  , getState, setState, getProps, createLeafElement, component)
import React.DOM (text)
import React.SyntheticEvent (target)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.TextField (textField')
import MaterialUI.Enums (primary)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)


type State =
  { open :: Boolean
  , name :: String
  , units :: String
  , description :: String
  }

initialState :: IxSig.IxSignal (read :: S.READ) TimeScale
             -> Effect State
initialState timeScaleSignal = do
  {name,units,description} <- IxSig.get timeScaleSignal
  pure
    { open: false
    , name
    , units
    , description
    }

timeScaleEditDialog :: { timeScaleSignal :: IxSig.IxSignal (read :: S.READ) TimeScale
                       , timeScaleEditQueues :: IOQueues Queue Unit (Maybe TimeScale)
                       } -> ReactElement
timeScaleEditDialog
  { timeScaleSignal
  , timeScaleEditQueues: IOQueues{input,output}
  } = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        styles :: _
        styles theme =
          { buttons:
            { zIndex: 2
            }
          }
        c' :: ReactClass {classes :: {buttons :: String}}
        c' = component "TimeScaleEdit" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handlerOpen :: _ -> Unit -> Effect Unit
          handlerOpen this _ = setState this {open: true}
          handlerChange :: _ -> TimeScale -> Effect Unit
          handlerChange this {name,units,description} = setState this {name,units,description}
      in  whileMountedOne input handlerOpen $
          whileMountedIx timeScaleSignal "TimeScaleEdit" handlerChange constructor
      where
        constructor this = do
          state <- initialState timeScaleSignal
          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state
            , render: do
              let close = do
                    state' <- initialState timeScaleSignal
                    setState this state'
                    put output Nothing
                  submit = do
                    {name,units,description} <- getState this
                    put output (Just {name,units,description})
                    setState this {open: false}
                  changeName e = do
                    t <- target e
                    setState this {name: (unsafeCoerce t).value}
                  changeUnits e = do
                    t <- target e
                    setState this {units: (unsafeCoerce t).value}
                  changeDescription e = do
                    t <- target e
                    setState this {description: (unsafeCoerce t).value}
              {open,name,units,description} <- getState this
              props <- getProps this
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "timeScaleedit-dialog-title"}
                  [ dialogTitle {id: "timeScaleedit-dialog-title"} [text "TimeScale"]
                  , dialogContent_
                    [ textField'
                      { label: "Name"
                      , value: name
                      , onChange: mkEffectFn1 changeName
                      , fullWidth: true
                      }
                    , textField'
                      { label: "Units"
                      , value: units
                      , onChange: mkEffectFn1 changeUnits
                      , fullWidth: true
                      }
                    , textField'
                      { label: "Description"
                      , value: description
                      , onChange: mkEffectFn1 changeDescription
                      , multiline: true
                      , fullWidth: true
                      , rowsMax: 4
                      }
                    ]
                  , dialogActions {className: props.classes.buttons}
                    [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                    , button
                      { onClick: mkEffectFn1 (const submit)
                      , color: primary
                      , autoFocus: true
                      } [text "Save"]
                    ]
                  ]
            }
