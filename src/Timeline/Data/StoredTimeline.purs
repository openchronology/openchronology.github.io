module Timeline.Data.StoredTimeline where


newtype StoredTimeline = StoredTimeline
  { timelineName :: String
  , events :: Array Event
  , axis :: 
  }


newtype Event = Event
  { title :: String
  , description :: String -- Markdown?
  , start :: Number -- post translation
  , end :: Number
  }
