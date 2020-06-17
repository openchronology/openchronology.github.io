module Components.Time.Span where

import Timeline.UI.Index.Unit (DecidedUnit(..))
import Timeline.UI.Index.Span (DecidedSpan(..), Span)
import Components.Time.Value
  ( DecidedIntermediaryValue(..)
  , valuePicker'
  )
import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Float.Parse (parseFloat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Exception (throw)
import React (ReactElement)
import React.DOM (div)
import React.DOM.Props (style) as RP

data DecidedIntermediarySpan
  = DecidedIntermediarySpanNumber (Span String)

derive instance genericDecidedIntermediarySpan :: Generic DecidedIntermediarySpan _

instance eqDecidedIntermediarySpan :: Eq DecidedIntermediarySpan where
  eq = genericEq

instance showDecidedIntermediarySpan :: Show DecidedIntermediarySpan where
  show = genericShow

getIntermediaryStart :: DecidedIntermediarySpan -> DecidedIntermediaryValue
getIntermediaryStart s = case s of
  DecidedIntermediarySpanNumber { start } -> DecidedIntermediaryValueNumber { value: start }

getIntermediaryStop :: DecidedIntermediarySpan -> DecidedIntermediaryValue
getIntermediaryStop s = case s of
  DecidedIntermediarySpanNumber { stop } -> DecidedIntermediaryValueNumber { value: stop }

initialDecidedIntermediarySpan :: DecidedUnit -> DecidedIntermediarySpan
initialDecidedIntermediarySpan u = case u of
  DecidedUnitNumber -> DecidedIntermediarySpanNumber { start: "", stop: "" }
  _ -> DecidedIntermediarySpanNumber { start: "", stop: "" } -- FIXME other units

intermediaryToSpan :: DecidedIntermediarySpan -> Maybe DecidedSpan
intermediaryToSpan i = case i of
  DecidedIntermediarySpanNumber { start, stop } -> case Tuple <$> parseFloat start <*> parseFloat stop of
    Nothing -> Nothing
    Just (Tuple start' stop') -> Just (DecidedSpanNumber { start: start', stop: stop' })

updateIntermediaryStart :: DecidedIntermediaryValue -> DecidedIntermediarySpan -> Maybe DecidedIntermediarySpan
updateIntermediaryStart v i = case Tuple v i of
  Tuple (DecidedIntermediaryValueNumber { value: start }) (DecidedIntermediarySpanNumber span) -> Just (DecidedIntermediarySpanNumber (span { start = start }))
  _ -> Nothing

updateIntermediaryStop :: DecidedIntermediaryValue -> DecidedIntermediarySpan -> Maybe DecidedIntermediarySpan
updateIntermediaryStop v i = case Tuple v i of
  Tuple (DecidedIntermediaryValueNumber { value: stop }) (DecidedIntermediarySpanNumber span) -> Just (DecidedIntermediarySpanNumber (span { stop = stop }))
  _ -> Nothing

spanPicker ::
  { onChangeIntermediarySpan :: DecidedIntermediarySpan -> Effect Unit
  , intermediarySpan :: DecidedIntermediarySpan
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
spanPicker { onChangeIntermediarySpan
, intermediarySpan
, decidedUnit
} =
  spanPicker'
    { onChangeIntermediarySpan
    , intermediarySpan
    , decidedUnit
    , disabledStart: false
    , disabledStop: false
    , preStart: []
    , preStop: []
    }

spanPicker' ::
  { onChangeIntermediarySpan :: DecidedIntermediarySpan -> Effect Unit
  , intermediarySpan :: DecidedIntermediarySpan
  , decidedUnit :: DecidedUnit
  , disabledStart :: Boolean
  , disabledStop :: Boolean
  , preStart :: Array ReactElement
  , preStop :: Array ReactElement
  } ->
  ReactElement
spanPicker' { onChangeIntermediarySpan
, intermediarySpan
, decidedUnit
, disabledStart
, disabledStop
, preStart
, preStop
} =
  let
    handleStart intermediaryStart = case updateIntermediaryStart intermediaryStart intermediarySpan of
      Nothing -> throw $ "Somehow got different units: " <> show { intermediaryStart, intermediarySpan }
      Just intermediarySpan' -> onChangeIntermediarySpan intermediarySpan'

    handleStop intermediaryStop = case updateIntermediaryStop intermediaryStop intermediarySpan of
      Nothing -> throw $ "Somehow got different units: " <> show { intermediaryStop, intermediarySpan }
      Just intermediarySpan' -> onChangeIntermediarySpan intermediarySpan'
  in
    div [ RP.style { display: "flex", flexDirection: "row" } ]
      [ div [ RP.style { flexGrow: 1 } ]
          $ preStart
          <> [ valuePicker'
                { onChangeIntermediaryValue: handleStart
                , intermediaryValue: getIntermediaryStart intermediarySpan
                , decidedUnit
                , decidedUnitLabel:
                    \u -> case u of
                      DecidedUnitNumber -> "Start"
                      DecidedUnitFoo -> "Start"
                , disabled: disabledStart
                , error:
                    case getIntermediaryStart intermediarySpan of
                      DecidedIntermediaryValueNumber { value: start' }
                        | start' == "" -> false
                        | otherwise -> case parseFloat start' of
                          Nothing -> true
                          Just start -> case getIntermediaryStop intermediarySpan of
                            DecidedIntermediaryValueNumber { value: stop' }
                              | stop' == "" -> false
                              | otherwise -> case parseFloat stop' of
                                Nothing -> false -- not the problem
                                Just stop -> start > stop
                , title:
                    case getIntermediaryStart intermediarySpan of
                      DecidedIntermediaryValueNumber { value: start' }
                        | start' == "" -> Nothing
                        | otherwise -> case parseFloat start' of
                          Nothing -> Just "Can't parse Number"
                          Just start -> case getIntermediaryStop intermediarySpan of
                            DecidedIntermediaryValueNumber { value: stop' }
                              | stop' == "" -> Nothing
                              | otherwise -> case parseFloat stop' of
                                Nothing -> Nothing -- not the problem
                                Just stop
                                  | start > stop -> Just "Start is greater than Stop"
                                  | otherwise -> Nothing
                }
            ]
      , div [ RP.style { flexGrow: 1 } ]
          $ preStop
          <> [ valuePicker'
                { onChangeIntermediaryValue: handleStop
                , intermediaryValue: getIntermediaryStop intermediarySpan
                , decidedUnit
                , decidedUnitLabel:
                    \u -> case u of
                      DecidedUnitNumber -> "Stop"
                      DecidedUnitFoo -> "Stop"
                , disabled: disabledStop
                , error:
                    case getIntermediaryStop intermediarySpan of
                      DecidedIntermediaryValueNumber { value: stop' }
                        | stop' == "" -> false
                        | otherwise -> case parseFloat stop' of
                          Nothing -> true
                          Just stop -> case getIntermediaryStart intermediarySpan of
                            DecidedIntermediaryValueNumber { value: start' }
                              | start' == "" -> false
                              | otherwise -> case parseFloat start' of
                                Nothing -> false -- not the problem
                                Just start -> start > stop
                , title:
                    case getIntermediaryStop intermediarySpan of
                      DecidedIntermediaryValueNumber { value: stop' }
                        | stop' == "" -> Nothing
                        | otherwise -> case parseFloat stop' of
                          Nothing -> Just "Can't parse Number"
                          Just stop -> case getIntermediaryStart intermediarySpan of
                            DecidedIntermediaryValueNumber { value: start' }
                              | start' == "" -> Nothing
                              | otherwise -> case parseFloat start' of
                                Nothing -> Nothing -- not the problem
                                Just start
                                  | start > stop -> Just "Stop is less than Start"
                                  | otherwise -> Nothing
                }
            ]
      ]
