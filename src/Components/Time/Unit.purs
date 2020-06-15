module Components.Time.Unit where

import Timeline.UI.Index.Unit (DecidedUnit(..))
import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2)
import Effect.Exception (throw)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , setState
  , getState
  )
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.FormControl (formControl_)
import MaterialUI.Select (select)
import MaterialUI.InputLabel (inputLabel)
import MaterialUI.MenuItem (menuItem)
import Unsafe.Coerce (unsafeCoerce)

type State
  = { value :: Maybe DecidedUnit
    }

initialState :: State
initialState = { value: Nothing }

unitPicker ::
  { onUnitPicked :: DecidedUnit -> Effect Unit
  , initialUnitPicked :: Maybe DecidedUnit
  } ->
  ReactElement
unitPicker { onUnitPicked, initialUnitPicked } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "UnitPicker" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' this =
    pure
      { componentDidMount: pure unit
      , componentWillUnmount: pure unit
      , state: initialState { value = initialUnitPicked }
      , render:
          do
            let
              handleChange e _ = do
                t <- target e
                let
                  val = (unsafeCoerce t).value
                val' <- case val of
                  "DecidedUnitNumber" -> pure DecidedUnitNumber
                  "DecidedUnitFoo" -> pure DecidedUnitFoo
                  _ -> throw $ "Can't determine shown DecidedUnit: " <> val
                setState this { value: Just val' }
                onUnitPicked val'
            { value } <- getState this
            pure
              $ formControl_
                  [ inputLabel { htmlFor: "unit-picker" } [ text "Units" ]
                  , select
                      { value: maybe "" show value
                      , onChange: mkEffectFn2 handleChange
                      , inputProps: { id: "unit-picker" }
                      }
                      $ let
                          pretty u = case u of
                            DecidedUnitNumber -> "Number"
                            DecidedUnitFoo -> "Foo"

                          makeMenuItem v = menuItem { value: show v } [ text (pretty v) ]
                        in
                          map makeMenuItem allUnits
                  ]
      }

  allUnits :: Array DecidedUnit
  allUnits = [ DecidedUnitNumber, DecidedUnitFoo ]
