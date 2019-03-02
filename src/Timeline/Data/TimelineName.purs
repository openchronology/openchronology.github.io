module Timeline.Data.TimelineName where


-- | Represents both the filename and the timeline's presented name
type TimelineName =
  { timelineName :: String
  , filename :: String
  }

-- TODO validate filename? Meh


-- | Chosen timeline name on boot, disregarding the shared signal
initialTimelineName :: TimelineName
initialTimelineName =
  { timelineName: ""
  , filename: "timeline" -- ^ Automatically appends `.och` on download
  }
