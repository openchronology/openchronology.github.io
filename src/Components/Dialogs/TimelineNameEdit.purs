module Components.Dialogs.TimelineNameEdit (timelineNameEditDialog) where

import Timeline.Data.TimelineName (TimelineName(..))
import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactClass
  , ReactClassConstructor
  , ReactElement
  , getState
  , setState
  , getProps
  , createLeafElement
  , component
  )
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
import MaterialUI.Typography (typography)
import MaterialUI.Enums (primary, secondary, body2)
import Queue.One (Queue, put)
import IOQueues (IOQueues(..))
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)

type State
  = { open :: Boolean
    , isEditable :: Boolean
    , title :: String
    , description :: String
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) TimelineName ->
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState timelineNameSignal settingsSignal = do
  TimelineName { title, description } <- IxSig.get timelineNameSignal
  Settings { isEditable } <- IxSig.get settingsSignal
  pure
    { open: false
    , isEditable
    , title
    , description
    }

timelineNameEditDialog ::
  { timelineNameSignal :: IxSig.IxSignal ( read :: S.READ ) TimelineName
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , timelineNameEditQueues :: IOQueues Queue Unit (Maybe TimelineName)
  } ->
  ReactElement
timelineNameEditDialog { timelineNameSignal
, settingsSignal
, timelineNameEditQueues: IOQueues { input, output }
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

    c' :: ReactClass { classes :: { buttons :: String } }
    c' = component "TimelineNameEdit" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }

      handlerChange :: _ -> TimelineName -> Effect Unit
      handlerChange this (TimelineName { title, description }) = setState this { title, description }

      handlerChangeEdit :: _ -> Settings -> Effect Unit
      handlerChangeEdit this (Settings { isEditable }) = setState this { isEditable }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx timelineNameSignal "TimelineNameEdit" handlerChange
        $ whileMountedIx settingsSignal "TimelineNameEdit" handlerChangeEdit constructor
    where
    constructor this = do
      state <- initialState timelineNameSignal settingsSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  state' <- initialState timelineNameSignal settingsSignal
                  setState this state'
                  put output Nothing

                submit = do
                  { title, description } <- getState this
                  put output (Just (TimelineName { title, description }))
                  setState this { open: false }

                changeTitle e = do
                  t <- target e
                  setState this { title: (unsafeCoerce t).value }

                changeDescription e = do
                  t <- target e
                  setState this { description: (unsafeCoerce t).value }
              { open, isEditable, title, description } <- getState this
              props <- getProps this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , "aria-labelledby": "timelinenameedit-dialog-title"
                    }
                $ let
                    editable =
                      [ dialogTitle { id: "timelinenameedit-dialog-title" } [ text "Timeline Name" ]
                      , dialogContent_
                          [ textField'
                              { label: "Title"
                              , value: title
                              , onChange: mkEffectFn1 changeTitle
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
                      , dialogActions { className: props.classes.buttons }
                          [ button { onClick: mkEffectFn1 (const close), color: primary } [ text "Cancel" ]
                          , button
                              { onClick: mkEffectFn1 (const submit)
                              , color: secondary
                              , autoFocus: true
                              }
                              [ text "Save" ]
                          ]
                      ]

                    notEditable =
                      [ dialogTitle { id: "timelinenameedit-dialog-title" } [ text title ]
                      , dialogContent_ -- FIXME use markdown
                          [ typography { gutterBottom: true, variant: body2 } [ text description ] ]
                      , dialogActions { className: props.classes.buttons }
                          [ button { onClick: mkEffectFn1 (const close), color: primary } [ text "Close" ] ]
                      ]
                  in
                    if isEditable then editable else notEditable
        }
