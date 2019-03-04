module Timeline.Data.StoredTimeline where


newtype StoredTimeline = StoredTimeline
  { timelineName :: String
  , root :: Array Timelines -- ^ Root timeline
  , axis :: Axis
  }



data TimelineAlignment
  = TimelineInline
  | TimelineBox

newtype Timeline = Timeline
  { title       :: String
  , description :: String
  , alignment   :: TimelineAlignment
  , start       :: Number
  , end         :: Number
  , children    :: Array Timeline
  }


newtype Axis = Axis
  { translation :: Number <-> BigNumber -- ? Units?
  , description :: String
  }
