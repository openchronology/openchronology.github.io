module Timeline.Data.TimeSpan where



-- | An inclusive span of time from `startIndex` to `stopIndex`.
-- |
-- | Defined over the user-level timescale `a`.
newtype TimeSpan a = TimeSpan
  { name        :: String
  , description :: String
  , startIndex  :: a
  , stopIndex   :: a
  }
