module Timeline.Data.Event where

-- | An event documented at time `index`.
-- |
-- | Defined over the user-level timescale `a`.
newtype Event a
  = Event
  { name :: String
  , description :: String
  , index :: a
  }
