module Components.Time.Value where

import Timeline.UI.Index (DecidedUnit(..), DecidedValue(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Float.Parse (parseFloat)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Timer (TimeoutId, setTimeout, clearTimeout)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , getState
  , setState
  )
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.TextField (textField')
import Unsafe.Coerce (unsafeCoerce)

type State
  = { value :: String
    , pending :: Maybe TimeoutId
    }

initialState :: State
initialState = { value: "", pending: Nothing }

valuePicker ::
  { onValuePicked :: DecidedValue -> Effect Unit
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
valuePicker { onValuePicked, decidedUnit } =
  valuePicker'
    { onValuePicked
    , decidedUnit
    , name: "Value"
    , decidedUnitLabel:
        \u -> case u of
          DecidedUnitNumber -> "Number"
          DecidedUnitFoo -> "Foo"
    }

valuePicker' ::
  { onValuePicked :: DecidedValue -> Effect Unit
  , decidedUnit :: DecidedUnit
  , name :: String
  , decidedUnitLabel :: DecidedUnit -> String
  } ->
  ReactElement
valuePicker' { onValuePicked, decidedUnit, name, decidedUnitLabel } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component (name <> "Picker") constructor

  constructor :: ReactClassConstructor _ State _
  constructor this =
    pure
      { componentDidMount: pure unit
      , componentWillUnmount: pure unit
      , state: initialState
      , render:
          do
            { value, pending } <- getState this
            let
              handleChange e = do
                t <- target e
                let
                  val = (unsafeCoerce t).value
                case pending of
                  Just id -> clearTimeout id
                  Nothing -> pure unit
                -- debouncer
                id <-
                  setTimeout 500 do
                    case decidedUnit of
                      DecidedUnitNumber -> do
                        case parseFloat val of
                          Nothing -> pure unit -- wait until it can parse
                          Just n -> onValuePicked (DecidedValueNumber n)
                        setState this { pending: Nothing }
                      _ -> pure unit -- FIXME
                setState this { value: val, pending: Just id }
            pure
              $ case decidedUnit of
                  DecidedUnitNumber ->
                    textField'
                      { label: decidedUnitLabel DecidedUnitNumber
                      , value
                      , onChange: mkEffectFn1 handleChange
                      , "type": "number"
                      }
                  _ -> text "" -- FIXME
      }
