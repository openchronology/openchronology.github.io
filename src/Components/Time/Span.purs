module Components.Time.Span where

import Timeline.UI.Index (DecidedUnit(..), DecidedValue(..), DecidedSpan(..), makeDecidedSpan)
import Components.Time.Value (valuePicker')
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Exception (throw)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , pureComponent
  , toElement
  )

type InternalState
  = { start :: Maybe DecidedValue
    , stop :: Maybe DecidedValue
    }

initialState :: InternalState
initialState = { start: Nothing, stop: Nothing }

spanPicker ::
  { onSpanPicked :: DecidedSpan -> Effect Unit
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
spanPicker { onSpanPicked, decidedUnit } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = pureComponent "SpanPicker" constructor

  constructor :: ReactClassConstructor _ {} _
  constructor this = do
    stateRef <- Ref.new initialState
    pure
      { state: {}
      , render:
          do
            let
              handleStart start = do
                { stop } <- Ref.read stateRef
                Ref.write { start: Just start, stop } stateRef
                case stop of
                  Just stop' -> case makeDecidedSpan { start, stop: stop' } of
                    Just span -> onSpanPicked span
                    Nothing -> throw $ "Somehow got different units in span: " <> show { start, stop: stop' }
                  Nothing -> pure unit

              handleStop stop = do
                { start } <- Ref.read stateRef
                Ref.write { start, stop: Just stop } stateRef
                case start of
                  Just start' -> case makeDecidedSpan { start: start', stop } of
                    Just span -> onSpanPicked span
                    Nothing -> throw $ "Somehow got different units in span: " <> show { start: start', stop }
                  Nothing -> pure unit
            pure
              $ toElement
                  [ valuePicker'
                      { onValuePicked: handleStart
                      , decidedUnit
                      , name: "Start"
                      , decidedUnitLabel:
                          \u -> case u of
                            DecidedUnitNumber -> "Start"
                      }
                  , valuePicker'
                      { onValuePicked: handleStop
                      , decidedUnit
                      , name: "Stop"
                      , decidedUnitLabel:
                          \u -> case u of
                            DecidedUnitNumber -> "Stop"
                      }
                  ]
      }
