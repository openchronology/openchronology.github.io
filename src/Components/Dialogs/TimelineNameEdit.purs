module Components.Dialogs.TimelineNameEdit (timelineNameEditDialog) where

import Timeline.Data.TimelineName (TimelineName)

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
  , title :: String
  , filename :: String
  , description :: String
  }

initialState :: IxSig.IxSignal (read :: S.READ) TimelineName
             -> Effect State
initialState timelineNameSignal = do
  {title,filename,description} <- IxSig.get timelineNameSignal
  pure
    { open: false
    , title
    , filename
    , description
    }

timelineNameEditDialog :: { timelineNameSignal :: IxSig.IxSignal (read :: S.READ) TimelineName
                          , timelineNameEditQueues :: IOQueues Queue Unit (Maybe TimelineName)
                          } -> ReactElement
timelineNameEditDialog
  { timelineNameSignal
  , timelineNameEditQueues: IOQueues{input,output}
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
        c' = component "TimelineNameEdit" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handlerOpen :: _ -> Unit -> Effect Unit
          handlerOpen this _ = setState this {open: true}
          handlerChange :: _ -> TimelineName -> Effect Unit
          handlerChange this {title,filename,description} = setState this {title,filename,description}
      in  whileMountedOne input handlerOpen $
          whileMountedIx timelineNameSignal "TimelineNameEdit" handlerChange constructor
      where
        constructor this = do
          state <- initialState timelineNameSignal
          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state
            , render: do
              let close = do
                    state' <- initialState timelineNameSignal
                    setState this state'
                    put output Nothing
                  submit = do
                    {title,filename,description} <- getState this
                    put output (Just {title,filename,description})
                    setState this {open: false}
                  changeTitle e = do
                    t <- target e
                    setState this {title: (unsafeCoerce t).value}
                  changeFilename e = do
                    t <- target e
                    setState this {filename: (unsafeCoerce t).value}
                  changeDescription e = do
                    t <- target e
                    setState this {description: (unsafeCoerce t).value}
              {open,title,filename,description} <- getState this
              props <- getProps this
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "timelinenameedit-dialog-title"}
                  [ dialogTitle {id: "timelinenameedit-dialog-title"} [text "Timeline Name"]
                  , dialogContent_
                    [ textField'
                      { label: "Title"
                      , value: title
                      , onChange: mkEffectFn1 changeTitle
                      , fullWidth: true
                      }
                    , textField'
                      { label: "Filename"
                      , value: filename
                      , onChange: mkEffectFn1 changeFilename
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
