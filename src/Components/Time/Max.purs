module Components.Time.Max where

import Components.Time.Value (DecidedIntermediaryValue, valuePicker')
import Timeline.UI.Index.Unit (DecidedUnit(..))
import Prelude (Unit)
import Data.Maybe (Maybe(..))
import Effect (Effect)
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
          DecidedUnitNumber -> "Max"
          DecidedUnitFoo -> "Max"
    , disabled: false
    , intermediaryValue
    , onChangeIntermediaryValue
    , error: false -- FIXME
    , title: Nothing
    }
