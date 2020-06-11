module Timeline.UI.Event where

import Timeline.UI.Index (DecidedValue)
import Prelude
import Data.Generic.Rep (class Generic)

-- | An event documented at time `index`.
-- |
-- | Defined over the user-level timescale `a`.
newtype Event
  = Event
  { name :: String
  , description :: String
  , time :: DecidedValue
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance showEvent :: Show Event

-- instance functorEvent :: Functor Event where
--   map f (Event x) = Event x {index = f x.index}
