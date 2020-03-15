module Timeline.Data.TimeScale where


type TimeScale =
  { name        :: String
  , units       :: String
  , description :: String
  -- , morphism :: Equation -- change this for different mappings - for now, we're linear
  }


initialTimeScale :: TimeScale
initialTimeScale =
  { name: "TimeScale Name"
  , units: "Years"
  , description: ""
  }
