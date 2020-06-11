module Timeline.UI.EventsAndTimeSpans where

import Timeline.UI.Event (Event)
import Timeline.UI.TimeSpan (TimeSpan)
import Prelude
import Data.Either (Either)

newtype EventsAndTimeSpans
  = EventsAndTimeSpans (Array (Either Event TimeSpan))
