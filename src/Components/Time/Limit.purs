module Components.Time.Limit where

import Components.Time.Bounds
  ( DecidedIntermediaryBounds(..)
  , initialDecidedIntermediaryBounds
  , boundsPicker'
  )
import Timeline.UI.Index.Unit (DecidedUnit)
import Timeline.UI.Index.Limit (DecidedLimit(..), Limit(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Float.Parse (parseFloat)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , pureComponent
  )
import MaterialUI.FormControlLabel (formControlLabel')
import MaterialUI.Switch (switch')

type DecidedIntermediaryLimit
  = { hasBegin :: Boolean
    , hasEnd :: Boolean
    , intermediaryBounds :: DecidedIntermediaryBounds
    }

initialDecidedIntermediaryLimit :: DecidedUnit -> DecidedIntermediaryLimit
initialDecidedIntermediaryLimit u =
  { hasBegin: true
  , hasEnd: true
  , intermediaryBounds: initialDecidedIntermediaryBounds u
  }

intermediaryToLimit :: DecidedIntermediaryLimit -> Maybe DecidedLimit
intermediaryToLimit { hasBegin, hasEnd, intermediaryBounds } = case intermediaryBounds of
  DecidedIntermediaryBoundsNumber { begin, end }
    | hasBegin && hasEnd -> case Tuple <$> parseFloat begin <*> parseFloat end of
      Nothing -> Nothing
      Just (Tuple begin' end') -> Just (DecidedLimitNumber (LimitBounds { begin: begin', end: end' }))
    | hasBegin && not hasEnd -> case parseFloat begin of
      Nothing -> Nothing
      Just begin' -> Just (DecidedLimitNumber (LimitMin { begin: begin' }))
    | not hasBegin && hasEnd -> case parseFloat end of
      Nothing -> Nothing
      Just end' -> Just (DecidedLimitNumber (LimitMax { end: end' }))
    | otherwise -> Nothing

limitPicker ::
  { onChangeIntermediaryLimit :: DecidedIntermediaryLimit -> Effect Unit
  , intermediaryLimit :: DecidedIntermediaryLimit
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
limitPicker { onChangeIntermediaryLimit, intermediaryLimit, decidedUnit } =
  let
    handleToggleBegin v = onChangeIntermediaryLimit $ intermediaryLimit { hasBegin = v }

    handleToggleEnd v = onChangeIntermediaryLimit $ intermediaryLimit { hasEnd = v }

    handleBounds b = onChangeIntermediaryLimit $ intermediaryLimit { intermediaryBounds = b }
  in
    boundsPicker'
      { onChangeIntermediaryBounds: handleBounds
      , intermediaryBounds: intermediaryLimit.intermediaryBounds
      , decidedUnit
      , disabledBegin: not intermediaryLimit.hasBegin
      , disabledEnd: not intermediaryLimit.hasEnd
      , preBegin:
          [ formControlLabel'
              { label: "Limit Beginning"
              , control:
                  switch'
                    { checked: intermediaryLimit.hasBegin
                    , onChange: mkEffectFn2 (const handleToggleBegin)
                    , disabled: not intermediaryLimit.hasEnd
                    }
              }
          ]
      , preEnd:
          [ formControlLabel'
              { label: "Limit End"
              , control:
                  switch'
                    { checked: intermediaryLimit.hasEnd
                    , onChange: mkEffectFn2 (const handleToggleEnd)
                    , disabled: not intermediaryLimit.hasBegin
                    }
              }
          ]
      }
