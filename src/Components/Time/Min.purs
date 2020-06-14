module Components.Time.Min where

import Prelude (Unit, (<<<))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Components.Time.Value (DecidedIntermediaryValue, valuePicker')
import Timeline.UI.Index (DecidedMin(..), DecidedValue(..), DecidedUnit(..), makeDecidedMin)
import React (ReactElement)

minPicker ::
  { decidedUnit :: DecidedUnit
  , intermediaryValue :: DecidedIntermediaryValue
  , onChangeIntermediaryValue :: DecidedIntermediaryValue -> Effect Unit
  } ->
  ReactElement
minPicker { decidedUnit, intermediaryValue, onChangeIntermediaryValue } =
  valuePicker'
    { decidedUnit
    , decidedUnitLabel:
        \u -> case u of
          DecidedUnitNumber -> "Min"
          DecidedUnitFoo -> "Min"
    , disabled: false
    , intermediaryValue
    , onChangeIntermediaryValue
    , error: false -- FIXME
    , title: Nothing
    }
