module Timeline.UI.EventOrTimeSpan where

import Timeline.UI.Event (Event)
import Timeline.UI.TimeSpan (TimeSpan)
import Data.Either (Either)

type EventOrTimeSpan
  = Either Event TimeSpan
