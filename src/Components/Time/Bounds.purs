module Components.Time.Bounds where

import Timeline.UI.Index.Unit (DecidedUnit(..))
import Timeline.UI.Index.Bounds (DecidedBounds(..), Bounds)
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

data DecidedIntermediaryBounds
  = DecidedIntermediaryBoundsNumber (Bounds String)

derive instance genericDecidedIntermediaryBounds :: Generic DecidedIntermediaryBounds _

instance eqDecidedIntermediaryBounds :: Eq DecidedIntermediaryBounds where
  eq = genericEq

instance showDecidedIntermediaryBounds :: Show DecidedIntermediaryBounds where
  show = genericShow

getIntermediaryBegin :: DecidedIntermediaryBounds -> DecidedIntermediaryValue
getIntermediaryBegin s = case s of
  DecidedIntermediaryBoundsNumber { begin } -> DecidedIntermediaryValueNumber { value: begin }

getIntermediaryEnd :: DecidedIntermediaryBounds -> DecidedIntermediaryValue
getIntermediaryEnd s = case s of
  DecidedIntermediaryBoundsNumber { end } -> DecidedIntermediaryValueNumber { value: end }

initialDecidedIntermediaryBounds :: DecidedUnit -> DecidedIntermediaryBounds
initialDecidedIntermediaryBounds u = case u of
  DecidedUnitNumber -> DecidedIntermediaryBoundsNumber { begin: "", end: "" }
  _ -> DecidedIntermediaryBoundsNumber { begin: "", end: "" } -- FIXME other units

intermediaryToBounds :: DecidedIntermediaryBounds -> Maybe DecidedBounds
intermediaryToBounds i = case i of
  DecidedIntermediaryBoundsNumber { begin, end } -> case Tuple <$> parseFloat begin <*> parseFloat end of
    Nothing -> Nothing
    Just (Tuple begin' end') -> Just (DecidedBoundsNumber { begin: begin', end: end' })

updateIntermediaryBegin :: DecidedIntermediaryValue -> DecidedIntermediaryBounds -> Maybe DecidedIntermediaryBounds
updateIntermediaryBegin v i = case Tuple v i of
  Tuple (DecidedIntermediaryValueNumber { value: begin }) (DecidedIntermediaryBoundsNumber bounds) -> Just (DecidedIntermediaryBoundsNumber (bounds { begin = begin }))
  _ -> Nothing

updateIntermediaryEnd :: DecidedIntermediaryValue -> DecidedIntermediaryBounds -> Maybe DecidedIntermediaryBounds
updateIntermediaryEnd v i = case Tuple v i of
  Tuple (DecidedIntermediaryValueNumber { value: end }) (DecidedIntermediaryBoundsNumber bounds) -> Just (DecidedIntermediaryBoundsNumber (bounds { end = end }))
  _ -> Nothing

boundsPicker ::
  { onChangeIntermediaryBounds :: DecidedIntermediaryBounds -> Effect Unit
  , intermediaryBounds :: DecidedIntermediaryBounds
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
boundsPicker { onChangeIntermediaryBounds
, intermediaryBounds
, decidedUnit
} =
  boundsPicker'
    { onChangeIntermediaryBounds
    , intermediaryBounds
    , decidedUnit
    , disabledBegin: false
    , disabledEnd: false
    , preBegin: []
    , preEnd: []
    }

boundsPicker' ::
  { onChangeIntermediaryBounds :: DecidedIntermediaryBounds -> Effect Unit
  , intermediaryBounds :: DecidedIntermediaryBounds
  , decidedUnit :: DecidedUnit
  , disabledBegin :: Boolean
  , disabledEnd :: Boolean
  , preBegin :: Array ReactElement
  , preEnd :: Array ReactElement
  } ->
  ReactElement
boundsPicker' { onChangeIntermediaryBounds
, intermediaryBounds
, decidedUnit
, disabledBegin
, disabledEnd
, preBegin
, preEnd
} =
  let
    handleBegin intermediaryBegin = case updateIntermediaryBegin intermediaryBegin intermediaryBounds of
      Nothing -> throw $ "Somehow got different units: " <> show { intermediaryBegin, intermediaryBounds }
      Just intermediaryBounds' -> onChangeIntermediaryBounds intermediaryBounds'

    handleEnd intermediaryEnd = case updateIntermediaryEnd intermediaryEnd intermediaryBounds of
      Nothing -> throw $ "Somehow got different units: " <> show { intermediaryEnd, intermediaryBounds }
      Just intermediaryBounds' -> onChangeIntermediaryBounds intermediaryBounds'
  in
    div [ RP.style { display: "flex", flexDirection: "row" } ]
      [ div [ RP.style { flexGrow: 1 } ]
          $ preBegin
          <> [ valuePicker'
                { onChangeIntermediaryValue: handleBegin
                , intermediaryValue: getIntermediaryBegin intermediaryBounds
                , decidedUnit
                , decidedUnitLabel:
                    \u -> case u of
                      DecidedUnitNumber -> "Begin"
                      DecidedUnitFoo -> "Begin"
                , disabled: disabledBegin
                , error:
                    case getIntermediaryBegin intermediaryBounds of
                      DecidedIntermediaryValueNumber { value: begin' }
                        | begin' == "" -> false
                        | otherwise -> case parseFloat begin' of
                          Nothing -> true
                          Just begin -> case getIntermediaryEnd intermediaryBounds of
                            DecidedIntermediaryValueNumber { value: end' }
                              | end' == "" -> false
                              | otherwise -> case parseFloat end' of
                                Nothing -> false -- not the problem
                                Just end -> begin > end
                , title:
                    case getIntermediaryBegin intermediaryBounds of
                      DecidedIntermediaryValueNumber { value: begin' }
                        | begin' == "" -> Nothing
                        | otherwise -> case parseFloat begin' of
                          Nothing -> Just "Can't parse Number"
                          Just begin -> case getIntermediaryEnd intermediaryBounds of
                            DecidedIntermediaryValueNumber { value: end' }
                              | end' == "" -> Nothing
                              | otherwise -> case parseFloat end' of
                                Nothing -> Nothing -- not the problem
                                Just end
                                  | begin > end -> Just "Beginning is greater than End"
                                  | otherwise -> Nothing
                }
            ]
      , div [ RP.style { flexGrow: 1 } ]
          $ preEnd
          <> [ valuePicker'
                { onChangeIntermediaryValue: handleEnd
                , intermediaryValue: getIntermediaryEnd intermediaryBounds
                , decidedUnit
                , decidedUnitLabel:
                    \u -> case u of
                      DecidedUnitNumber -> "End"
                      DecidedUnitFoo -> "End"
                , disabled: disabledEnd
                , error:
                    case getIntermediaryEnd intermediaryBounds of
                      DecidedIntermediaryValueNumber { value: end' }
                        | end' == "" -> false
                        | otherwise -> case parseFloat end' of
                          Nothing -> true
                          Just end -> case getIntermediaryBegin intermediaryBounds of
                            DecidedIntermediaryValueNumber { value: begin' }
                              | begin' == "" -> false
                              | otherwise -> case parseFloat begin' of
                                Nothing -> false -- not the problem
                                Just begin -> begin > end
                , title:
                    case getIntermediaryEnd intermediaryBounds of
                      DecidedIntermediaryValueNumber { value: end' }
                        | end' == "" -> Nothing
                        | otherwise -> case parseFloat end' of
                          Nothing -> Just "Can't parse Number"
                          Just end -> case getIntermediaryBegin intermediaryBounds of
                            DecidedIntermediaryValueNumber { value: begin' }
                              | begin' == "" -> Nothing
                              | otherwise -> case parseFloat begin' of
                                Nothing -> Nothing -- not the problem
                                Just begin
                                  | begin > end -> Just "End is less than Beginning"
                                  | otherwise -> Nothing
                }
            ]
      ]
