module Timeline.Data.TimelineName where


-- | Represents both the filename and the timeline's presented name
type TimelineName =
  { title       :: String
  , filename    :: String
  , description :: String
  }

-- TODO validate filename? Meh


-- | Chosen timeline name on boot, disregarding the shared signal
initialTimelineName :: TimelineName
initialTimelineName =
  { title: "Timeline Name"
  , filename: "timeline" -- ^ Automatically appends `.och` on download
  , description: ""
  }
