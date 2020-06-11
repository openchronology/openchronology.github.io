module Components.Dialogs.NewOrEditTimeline where

import Timeline.UI.Timeline (Timeline(..))
import Settings (Settings (..))
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Timer (setTimeout)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , setState
  , getState
  , getProps
  )
import React.DOM (text, br)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import React.SyntheticEvent (target)
import MaterialUI.Colors (red)
import MaterialUI.Button (button)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.TextField (textField')
import MaterialUI.Styles (withStyles)
import MaterialUI.Theme (Theme)
import MaterialUI.Enums (secondary)
import MaterialUI.Markdown (markdown)
import IOQueues (IOQueues(..))
import Queue.One (Queue, put)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)

data NewOrEditTimelineResult
  = DeleteTimeline
  | NewOrEditTimeline Timeline

type State
  = { open :: Boolean
    , name :: String
    , description :: String
    , new :: Boolean
    , isEditable :: Boolean
    }

initialState :: IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState settingsSignal = do
  Settings {isEditable} <- IxSig.get settingsSignal
  pure
    { open: false
    , name: ""
    , description: ""
    , new: true
    , isEditable
    }

styles :: Theme -> _
styles theme =
  { deleteButton:
      { color: theme.palette.getContrastText red."500"
      , backgroundColor: red."500"
      , "&:hover":
          { backgroundColor: red."700"
          }
      }
  }

newOrEditTimelineDialog ::
  { onDelete :: Effect Unit -> Effect Unit
  , newOrEditTimelineQueues :: IOQueues Queue (Maybe Timeline) (Maybe NewOrEditTimelineResult)
  , settingsSignal :: IxSig.IxSignal (read :: S.READ) Settings
  } ->
  ReactElement
newOrEditTimelineDialog { onDelete
, newOrEditTimelineQueues: IOQueues { input, output }
, settingsSignal
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' :: ReactClass { classes :: { deleteButton :: String } }
    c' = component "NewOrEditTimelineDialog" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Maybe Timeline -> Effect Unit
      handlerOpen this mT = do
        _ <- setTimeout 250 (setState this { open: true })
        case mT of
          Nothing -> do
            state <- initialState settingsSignal
            setState this state
          Just (Timeline { name, description }) -> do
            setState this
              { name
              , description
              , new: false
              }

      handlerChangeEdit :: _ -> Settings -> Effect Unit
      handlerChangeEdit this (Settings {isEditable}) =
        setState this {isEditable}
    in
      whileMountedOne input handlerOpen $
      whileMountedIx settingsSignal "NewOrEditTimelineDialog" handlerChangeEdit constructor
    where
    constructor this = do
      state <- initialState settingsSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  setState this { open: false }
                  put output Nothing

                submit = do
                  { name, description } <- getState this
                  put output (Just (NewOrEditTimeline (Timeline { name, description })))
                  setState this { open: false }

                delete =
                  onDelete do
                    put output (Just DeleteTimeline)
                    setState this { open: false}

                changeName e = do
                  t <- target e
                  setState this { name: (unsafeCoerce t).value }

                changeDescription e = do
                  t <- target e
                  setState this { description: (unsafeCoerce t).value }
              { open, name, description, new, isEditable } <- getState this
              props <- getProps this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , "aria-labelledby": "newOrEditTimeline-dialog-title"
                    , fullWidth: true
                    } $
                    if isEditable
                      then
                        [ dialogTitle { id: "newOrEditTimeline-dialog-title" }
                            [ text $ if new then "New Timeline" else "Edit Timeline" ]
                        , dialogContent_ $
                            [ textField'
                                { label: "Name"
                                , value: name
                                , onChange: mkEffectFn1 changeName
                                , fullWidth: true
                                }
                            , textField'
                                { label: "Description"
                                , value: description
                                , onChange: mkEffectFn1 changeDescription
                                , multiline: true
                                , fullWidth: true
                                , rows: 4
                                }
                            ]
                          <> if new then
                              []
                            else
                              [ br []
                              , br []
                              , button
                                  { onClick: mkEffectFn1 (const delete)
                                  , className: props.classes.deleteButton
                                  , fullWidth: true
                                  }
                                  [ text "Delete" ]
                              ]
                        , dialogActions_
                            [ button { onClick: mkEffectFn1 (const close) } [ text "Cancel" ]
                            , button
                                { onClick: mkEffectFn1 (const submit)
                                , color: secondary
                                , autoFocus: true
                                }
                                [ text $ if new then "Submit" else "Save" ]
                            ]
                        ]
                      else
                        [ dialogTitle { id: "newOrEditTimeline-dialog-title" }
                            [ text name ]
                        , dialogContent_ $
                            [ markdown description ]
                        , dialogActions_
                            [ button { onClick: mkEffectFn1 (const close) } [ text "Close" ] ]
                        ]
        }
