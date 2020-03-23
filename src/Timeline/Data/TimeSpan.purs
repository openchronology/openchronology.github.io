module Timeline.Data.TimeSpan where


-- | Defined over the user-level timescale
newtype Event a = Event
  { name        :: String
  , description :: String
  , time        :: a
  }


-- | Defined over the user-level timescale
newtype TimeSpan a = TimeSpan
  { name        :: String
  , description :: String
  , startTime   :: a
  , endTime     :: a
  }
