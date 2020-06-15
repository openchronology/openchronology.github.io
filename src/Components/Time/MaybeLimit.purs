module Components.Time.MaybeLimit where

import Components.Time.Bounds
  ( DecidedIntermediaryBounds(..)
  , initialDecidedIntermediaryBounds
  , boundsPicker'
  )
import Timeline.UI.Index
  ( DecidedUnit
  , DecidedMaybeLimit(..)
  , MaybeLimit(..)
  )
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

type DecidedIntermediaryMaybeLimit
  = { hasBegin :: Boolean
    , hasEnd :: Boolean
    , intermediaryBounds :: DecidedIntermediaryBounds
    }

initialDecidedIntermediaryMaybeLimit :: DecidedUnit -> DecidedIntermediaryMaybeLimit
initialDecidedIntermediaryMaybeLimit u =
  { hasBegin: true
  , hasEnd: true
  , intermediaryBounds: initialDecidedIntermediaryBounds u
  }

intermediaryToMaybeLimit :: DecidedIntermediaryMaybeLimit -> Maybe DecidedMaybeLimit
intermediaryToMaybeLimit { hasBegin, hasEnd, intermediaryBounds } = case intermediaryBounds of
  DecidedIntermediaryBoundsNumber { begin, end }
    | hasBegin && hasEnd -> case Tuple <$> parseFloat begin <*> parseFloat end of
      Nothing -> Nothing
      Just (Tuple begin' end') -> Just (DecidedMaybeLimitNumber (JustLimitBounds { begin: begin', end: end' }))
    | hasBegin && not hasEnd -> case parseFloat begin of
      Nothing -> Nothing
      Just begin' -> Just (DecidedMaybeLimitNumber (JustLimitMin { begin: begin' }))
    | not hasBegin && hasEnd -> case parseFloat end of
      Nothing -> Nothing
      Just end' -> Just (DecidedMaybeLimitNumber (JustLimitMax { end: end' }))
    | otherwise -> Just (DecidedMaybeLimitNumber NothingLimit)

maybeLimitPicker ::
  { onChangeIntermediaryMaybeLimit :: DecidedIntermediaryMaybeLimit -> Effect Unit
  , intermediaryMaybeLimit :: DecidedIntermediaryMaybeLimit
  , decidedUnit :: DecidedUnit
  } ->
  ReactElement
maybeLimitPicker { onChangeIntermediaryMaybeLimit
, intermediaryMaybeLimit
, decidedUnit
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = pureComponent "MaybeLimitPicker" constructor

  constructor :: ReactClassConstructor _ {} _
  constructor this = do
    pure
      { state: {}
      , render:
          do
            let
              handleToggleBegin v = onChangeIntermediaryMaybeLimit $ intermediaryMaybeLimit { hasBegin = v }

              handleToggleEnd v = onChangeIntermediaryMaybeLimit $ intermediaryMaybeLimit { hasEnd = v }

              handleBounds b = onChangeIntermediaryMaybeLimit $ intermediaryMaybeLimit { intermediaryBounds = b }
            pure
              $ boundsPicker'
                  { onChangeIntermediaryBounds: handleBounds
                  , intermediaryBounds: intermediaryMaybeLimit.intermediaryBounds
                  , decidedUnit
                  , disabledBegin: not intermediaryMaybeLimit.hasBegin
                  , disabledEnd: not intermediaryMaybeLimit.hasEnd
                  , preBegin:
                      [ formControlLabel'
                          { label: "Limit Beginning"
                          , control:
                              switch'
                                { checked: intermediaryMaybeLimit.hasBegin
                                , onChange: mkEffectFn2 (const handleToggleBegin)
                                }
                          }
                      ]
                  , preEnd:
                      [ formControlLabel'
                          { label: "Limit End"
                          , control:
                              switch'
                                { checked: intermediaryMaybeLimit.hasEnd
                                , onChange: mkEffectFn2 (const handleToggleEnd)
                                }
                          }
                      ]
                  }
      }
