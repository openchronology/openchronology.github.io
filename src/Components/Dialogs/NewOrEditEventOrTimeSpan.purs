module Components.Dialogs.NewOrEditEventOrTimeSpan where

import Timeline.Time.Unit (DecidedUnit)
import Timeline.Time.MaybeLimit (getMaybeLimitDecidedUnit)
import Timeline.UI.TimeScale (TimeScale(..))
import Timeline.UI.Event (Event(..))
import Timeline.UI.TimeSpan (TimeSpan(..))
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan(..))
import Timeline.UI.Settings (Settings(..))
import Components.Time.Value
  ( DecidedIntermediaryValue
  , initialDecidedIntermediaryValue
  , intermediaryToValue
  , valuePicker
  )
import Components.Time.Span
  ( DecidedIntermediarySpan
  , initialDecidedIntermediarySpan
  , intermediaryToSpan
  , spanPicker
  )
import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Either (Either(..))
import Data.UUID (UUID)
import Data.UUID (genUUID) as UUID
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
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
import MaterialUI.AppBar (appBar)
import MaterialUI.Tabs (tabs)
import MaterialUI.Tab (tab')
import MaterialUI.FormControlLabel (formControlLabel')
import MaterialUI.Switch (switch')
import MaterialUI.Styles (withStyles)
import MaterialUI.Theme (Theme)
import MaterialUI.Enums (secondary, primary, static, default, fullWidth)
import MaterialUI.Markdown (markdown)
import IOQueues (IOQueues(..))
import Queue.One (Queue, put)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

data EventOrTimeSpanTab
  = EventTab
  | TimeSpanTab

data NewOrEditEventOrTimeSpanResult
  = DeleteEventOrTimeSpan
  | NewOrEditEventOrTimeSpan EventOrTimeSpan

type NewOrEditEventOrTimeSpan
  = { eventOrTimeSpan :: EventOrTimeSpan
    , onDelete :: Effect Unit -> Effect Unit
    }

-- | Both are contained at the same time, to allow for copy-and-pasting
-- | during an edit.
type State
  = { open :: Boolean
    , tab :: EventOrTimeSpanTab
    , decidedUnit :: DecidedUnit -- FIXME is the time / span within bounds?
    , eventName :: String
    , eventDescription :: String
    , eventTime :: DecidedIntermediaryValue
    , timeSpanName :: String
    , timeSpanDescription :: String
    , timeSpanSpan :: DecidedIntermediarySpan
    , timeSpanTimeSpace :: Maybe UUID -- TODO Lookup from ExploreTimeSpaces
    , new :: Boolean
    , isEditable :: Boolean
    , onDelete :: Effect Unit -> Effect Unit
    }

-- FIXME get decidedUnit from time scale signal
initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  IxSig.IxSignal ( read :: S.READ ) TimeScale ->
  Effect State
initialState settingsSignal timeScaleSignal = do
  Settings { isEditable } <- IxSig.get settingsSignal
  TimeScale { limit } <- IxSig.get timeScaleSignal
  let
    decidedUnit = getMaybeLimitDecidedUnit limit
  pure
    { open: false
    , tab: EventTab
    , decidedUnit
    , eventName: ""
    , eventDescription: ""
    , eventTime: initialDecidedIntermediaryValue decidedUnit
    , timeSpanName: ""
    , timeSpanDescription: ""
    , timeSpanSpan: initialDecidedIntermediarySpan decidedUnit
    , timeSpanTimeSpace: Nothing -- TODO Lookup from ExploreTimeSpaces
    , new: true -- Assumed new
    , isEditable
    , onDelete: \x -> x
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

newOrEditEventOrTimeSpanDialog ::
  { newOrEditEventOrTimeSpanQueues :: IOQueues Queue (Maybe NewOrEditEventOrTimeSpan) (Maybe NewOrEditEventOrTimeSpanResult)
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , timeScaleSignal :: IxSig.IxSignal ( read :: S.READ ) TimeScale
  } ->
  ReactElement
newOrEditEventOrTimeSpanDialog { newOrEditEventOrTimeSpanQueues: IOQueues { input, output }
, settingsSignal
, timeScaleSignal
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' :: ReactClass { classes :: { deleteButton :: String } }
    c' = component "NewOrEditEventOrTimeSpanDialog" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Maybe NewOrEditEventOrTimeSpan -> Effect Unit
      handlerOpen this mT = do
        _ <- setTimeout 250 (setState this { open: true })
        case mT of
          Nothing -> do
            state <- initialState settingsSignal timeScaleSignal
            setState this state
          Just { eventOrTimeSpan: EventOrTimeSpan eOrT, onDelete } -> case eOrT of
            Left (Event { name, description }) ->
              setState this
                { tab: EventTab
                , eventName: name
                , eventDescription: description
                , new: false
                , onDelete
                }
            Right (TimeSpan { name, description }) ->  -- TODO rest of data
              setState this
                { tab: TimeSpanTab
                , timeSpanName: name
                , timeSpanDescription: description
                , new: false
                , onDelete
                }

      handlerChangeEdit :: _ -> Settings -> Effect Unit
      handlerChangeEdit this (Settings { isEditable }) = setState this { isEditable }

      handlerChangeTimeScale :: _ -> TimeScale -> Effect Unit
      handlerChangeTimeScale this (TimeScale { limit }) = setState this { decidedUnit: getMaybeLimitDecidedUnit limit }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx settingsSignal "NewOrEditEventOrTimeSpanDialog" handlerChangeEdit
        $ whileMountedIx timeScaleSignal "NewOrEditEventOrTimeSpanDialog" handlerChangeTimeScale constructor
    where
    constructor this = do
      state <- initialState settingsSignal timeScaleSignal
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
                  { tab } <- getState this
                  case tab of
                    EventTab -> do
                      { eventName
                      , eventDescription
                      , eventTime
                      } <-
                        getState this
                      case intermediaryToValue eventTime of
                        Nothing -> pure unit -- FIXME throw snackbar
                        Just time -> do
                          id <- UUID.genUUID -- FIXME get from edited?
                          put output $ Just $ NewOrEditEventOrTimeSpan
                            $ EventOrTimeSpan
                            $ Left
                            $ Event
                                { name: eventName
                                , description: eventDescription
                                , time
                                , id
                                }
                    TimeSpanTab -> do
                      { timeSpanName
                      , timeSpanDescription
                      , timeSpanSpan
                      , timeSpanTimeSpace
                      } <-
                        getState this
                      case intermediaryToSpan timeSpanSpan of
                        Nothing -> pure unit -- FIXME throw snackbar
                        Just span -> do
                          id <- UUID.genUUID -- FIXME get from edited?
                          put output $ Just $ NewOrEditEventOrTimeSpan
                            $ EventOrTimeSpan
                            $ Right
                            $ TimeSpan
                                { name: timeSpanName
                                , description: timeSpanDescription
                                , span
                                , timeSpace: timeSpanTimeSpace
                                , id
                                }
                  setState this { open: false }

                delete = do
                  { onDelete } <- getState this
                  onDelete do
                    put output (Just DeleteEventOrTimeSpan)
                    setState this { open: false, onDelete: \x -> x }

                changeTab _ v =
                  setState this
                    { tab:
                        unsafePartial
                          $ case unsafeCoerce v of
                              0 -> EventTab
                              1 -> TimeSpanTab
                    }

                changeEventName e = do
                  t <- target e
                  setState this { eventName: (unsafeCoerce t).value }

                changeEventDescription e = do
                  t <- target e
                  setState this { eventDescription: (unsafeCoerce t).value }

                changeEventTime eventTime = setState this { eventTime }

                changeTimeSpanName e = do
                  t <- target e
                  setState this { timeSpanName: (unsafeCoerce t).value }

                changeTimeSpanDescription e = do
                  t <- target e
                  setState this { timeSpanDescription: (unsafeCoerce t).value }

                changeTimeSpanSpan timeSpanSpan = setState this { timeSpanSpan }
              { open
              , tab
              , decidedUnit
              , eventName
              , eventDescription
              , eventTime
              , timeSpanName
              , timeSpanDescription
              , timeSpanSpan
              , timeSpanTimeSpace
              , new
              , isEditable
              } <-
                getState this
              props <- getProps this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , "aria-labelledby": "newOrEditEventOrTimeSpan-dialog-title"
                    , fullWidth: true
                    }
                $ if isEditable then
                    [ dialogTitle { id: "newOrEditEventOrTimeSpan-dialog-title" }
                        [ text
                            $ (if new then "New " else "Edit ")
                            <> ( case tab of
                                  EventTab -> "Event"
                                  TimeSpanTab -> "TimeSpan"
                              )
                        ]
                    , dialogContent_ -- TODO tabs, and other content
                        $ [ appBar
                              { position: static
                              , color: default
                              }
                              [ tabs
                                  { indicatorColor: primary
                                  , textColor: primary
                                  , variant: fullWidth
                                  , value:
                                      case tab of
                                        EventTab -> 0
                                        TimeSpanTab -> 1
                                  , onChange: mkEffectFn2 changeTab
                                  }
                                  [ tab' { label: "Event" }
                                  , tab' { label: "TimeSpan" }
                                  ]
                              ]
                          , br []
                          ]
                        <> case tab of
                            EventTab ->
                              [ textField'
                                  { label: "Name"
                                  , value: eventName
                                  , onChange: mkEffectFn1 changeEventName
                                  , fullWidth: true
                                  }
                              , textField'
                                  { label: "Description"
                                  , value: eventDescription
                                  , onChange: mkEffectFn1 changeEventDescription
                                  , multiline: true
                                  , fullWidth: true
                                  , rows: 4
                                  }
                              , valuePicker
                                  { decidedUnit
                                  , intermediaryValue: eventTime
                                  , onChangeIntermediaryValue: changeEventTime
                                  }
                              ]
                            TimeSpanTab ->
                              [ textField'
                                  { label: "Name"
                                  , value: timeSpanName
                                  , onChange: mkEffectFn1 changeTimeSpanName
                                  , fullWidth: true
                                  }
                              , textField'
                                  { label: "Description"
                                  , value: timeSpanDescription
                                  , onChange: mkEffectFn1 changeTimeSpanDescription
                                  , multiline: true
                                  , fullWidth: true
                                  , rows: 4
                                  }
                              , spanPicker
                                  { decidedUnit
                                  , intermediarySpan: timeSpanSpan
                                  , onChangeIntermediarySpan: changeTimeSpanSpan
                                  }
                              , formControlLabel'
                                  { control:
                                      switch'
                                        { checked: isJust timeSpanTimeSpace
                                        -- , onChange: 
                                        } -- TODO add "new time space" / "delete time space?" dialogs
                                  , label: "Owns a TimeSpace"
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
                            [ text $ if new then "Submit" else "Save" ] -- FIXME disable submit if form is bad
                        ]
                    ]
                  else
                    [ dialogTitle { id: "newOrEditEventOrTimeSpan-dialog-title" }
                        [ text
                            $ case tab of
                                EventTab -> eventName
                                TimeSpanTab -> timeSpanName
                        ]
                    , dialogContent_
                        [ markdown
                            $ case tab of
                                EventTab -> eventDescription
                                TimeSpanTab -> timeSpanDescription
                        ]
                    , dialogActions_
                        [ button { onClick: mkEffectFn1 (const close) } [ text "Close" ] ]
                    ]
        }
