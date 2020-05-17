module Timeline.UI.StoredTimeline where

import Prelude
import Data.Map (Map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)

newtype StoredTimeline
  = StoredTimeline
  { title :: String
  , description :: String
  , events :: Map (Occurrence Number) Event
  , axis :: Axis
  }

-- this design is for the future idea of heirarchical timelines - out of the scope of the first release.
-- newtype Timeline = Timeline
--   { title       :: String
--   , description :: String
--   , alignment   :: TimelineAlignment
--   , start       :: Number
--   , end         :: Number
--   , children    :: Array Timeline
--   }
-- data TimelineAlignment
--   = TimelineInline
--  -- | TimelineBox
data Occurrence a
  = Point a
  | Span
    { begin :: a
    , end :: a
    }

derive instance genericOccurrence :: Generic a a' => Generic (Occurrence a) _

instance eqOccurrence :: (Generic a a', Eq a) => Eq (Occurrence a) where
  eq = genericEq

instance ordOccurrence :: (Generic a a', Ord a) => Ord (Occurrence a) where
  compare = genericCompare

instance showOccurrence :: (Generic a a', Show a) => Show (Occurrence a) where
  show = genericShow

newtype Event
  = Event
  { name :: String
  , description :: String
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance showEvent :: Show Event

newtype Axis
  = Axis
  -- { translation :: Number <-> BigNumber -- ? Units?
  { description :: String
  }
