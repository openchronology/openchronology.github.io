module Components.Time.Bounds where

import Timeline.UI.Index (DecidedUnit (..), DecidedValue (..), DecidedBounds (..), makeDecidedBounds)
import Components.Time.Value (valuePicker')

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Exception (throw)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , createLeafElement, pureComponent, toElement
  )


type InternalState =
  { begin :: Maybe DecidedValue
  , end :: Maybe DecidedValue
  }

initialState :: InternalState
initialState = { begin: Nothing, end: Nothing }

spanPicker :: { onBoundsPicked :: DecidedBounds -> Effect Unit
              , decidedUnit :: DecidedUnit
              } -> ReactElement
spanPicker {onBoundsPicked,decidedUnit} = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent "BoundsPicker" constructor

    constructor :: ReactClassConstructor _ {} _
    constructor this = do
      stateRef <- Ref.new initialState
      pure
        { state: {}
        , render: do
          let handleBegin begin = do
                {end} <- Ref.read stateRef
                Ref.write {begin: Just begin, end} stateRef
                case end of
                  Just end' -> case makeDecidedBounds {begin, end: end'} of
                    Just span -> onBoundsPicked span
                    Nothing -> throw $ "Somehow got different units in span: " <> show {begin, end: end'}
                  Nothing -> pure unit
              handleEnd end = do
                {begin} <- Ref.read stateRef
                Ref.write {begin, end: Just end} stateRef
                case begin of
                  Just begin' -> case makeDecidedBounds {begin: begin', end} of
                    Just span -> onBoundsPicked span
                    Nothing -> throw $ "Somehow got different units in span: " <> show {begin: begin', end}
                  Nothing -> pure unit
          pure $ toElement
            [ valuePicker'
                { onValuePicked: handleBegin
                , decidedUnit
                , name: "Begin"
                , decidedUnitLabel: \u -> case u of
                  DecidedUnitNumber -> "Begin"
                }
            , valuePicker'
                { onValuePicked: handleEnd
                , decidedUnit
                , name: "End"
                , decidedUnitLabel: \u -> case u of
                  DecidedUnitNumber -> "End"
                }
            ]
        }
