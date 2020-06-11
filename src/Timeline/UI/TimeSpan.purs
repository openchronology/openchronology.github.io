module Timeline.UI.TimeSpan where

import Timeline.UI.Index (DecidedSpan)
import Prelude
import Data.Maybe (Maybe)
import Data.IxSet.Demi (Index)
import Data.Generic.Rep (class Generic)

-- | An inclusive span of time from `startIndex` to `stopIndex`.
-- |
-- | Defined over the user-level timescale `a`.
newtype TimeSpan
  = TimeSpan
  { name :: String
  , description :: String
  , span :: DecidedSpan
  , timeSpace :: Maybe Index
  }

derive instance genericTimeSpan :: Generic TimeSpan _

derive newtype instance eqTimeSpan :: Eq TimeSpan

derive newtype instance showTimeSpan :: Show TimeSpan
