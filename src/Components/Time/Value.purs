module Components.Time.Value where

import Timeline.Time.Unit (DecidedUnit(..))
import Timeline.Time.Value (DecidedValue(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Float.Parse (parseFloat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React (ReactElement)
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.TextField (textField')
import Unsafe.Coerce (unsafeCoerce)

-- | Used to maintain the state of the picker at a higher level
data DecidedIntermediaryValue
  = DecidedIntermediaryValueNumber { value :: String }

derive instance genericDecidedIntermediaryValue :: Generic DecidedIntermediaryValue _

instance eqDecidedIntermediaryValue :: Eq DecidedIntermediaryValue where
  eq = genericEq

instance showDecidedIntermediaryValue :: Show DecidedIntermediaryValue where
  show = genericShow

initialDecidedIntermediaryValue :: DecidedUnit -> DecidedIntermediaryValue
initialDecidedIntermediaryValue u = case u of
  DecidedUnitNumber -> DecidedIntermediaryValueNumber { value: "" }
  _ -> DecidedIntermediaryValueNumber { value: "" } -- FIXME other units

intermediaryToValue :: DecidedIntermediaryValue -> Maybe DecidedValue
intermediaryToValue i = case i of
  DecidedIntermediaryValueNumber { value } -> case parseFloat value of
    Nothing -> Nothing
    Just n -> Just (DecidedValueNumber n)

valuePicker ::
  { decidedUnit :: DecidedUnit
  , intermediaryValue :: DecidedIntermediaryValue
  , onChangeIntermediaryValue :: DecidedIntermediaryValue -> Effect Unit
  } ->
  ReactElement
valuePicker { decidedUnit, intermediaryValue, onChangeIntermediaryValue } =
  valuePicker'
    { decidedUnit
    , decidedUnitLabel:
        \u -> case u of
          DecidedUnitNumber -> "Number"
          DecidedUnitFoo -> "Foo"
    , disabled: false
    , intermediaryValue
    , onChangeIntermediaryValue
    , error: false
    , title: Nothing
    }

valuePicker' ::
  { decidedUnit :: DecidedUnit
  , decidedUnitLabel :: DecidedUnit -> String
  , disabled :: Boolean
  , intermediaryValue :: DecidedIntermediaryValue
  , onChangeIntermediaryValue :: DecidedIntermediaryValue -> Effect Unit
  , error :: Boolean
  , title :: Maybe String
  } ->
  ReactElement
valuePicker' { decidedUnit
, decidedUnitLabel
, disabled
, intermediaryValue
, onChangeIntermediaryValue
, error
, title
} = case decidedUnit of
  DecidedUnitNumber ->
    let
      handleChange e = do
        t <- target e
        let
          value = (unsafeCoerce t).value
        onChangeIntermediaryValue (DecidedIntermediaryValueNumber { value })
    in
      case title of
        Nothing ->
          textField'
            { label: decidedUnitLabel DecidedUnitNumber
            , value:
                case intermediaryValue of
                  DecidedIntermediaryValueNumber { value } -> value
                  _ -> "" -- FIXME what if it's the bad unit?
            , onChange: mkEffectFn1 handleChange
            , "type": "number"
            , disabled
            , error
            , fullWidth: true
            }
        Just title' ->
          textField'
            { label: decidedUnitLabel DecidedUnitNumber
            , value:
                case intermediaryValue of
                  DecidedIntermediaryValueNumber { value } -> value
                  _ -> "" -- FIXME what if it's the bad unit?
            , onChange: mkEffectFn1 handleChange
            , "type": "number"
            , disabled
            , error
            , title: title'
            , fullWidth: true
            }
  _ -> text "" -- FIXME other units
