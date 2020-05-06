## Module Timeline.Data.StoredTimeline

#### `StoredTimeline`

``` purescript
newtype StoredTimeline
  = StoredTimeline { axis :: Axis, description :: String, events :: Map (Occurrence Number) Event, title :: String }
```

#### `Occurrence`

``` purescript
data Occurrence a
  = Point a
  | Span { begin :: a, end :: a }
```

##### Instances
``` purescript
(Generic a a') => Generic (Occurrence a) _
(Generic a a', Eq a) => Eq (Occurrence a)
(Generic a a', Ord a) => Ord (Occurrence a)
(Generic a a', Show a) => Show (Occurrence a)
```

#### `Event`

``` purescript
newtype Event
  = Event { description :: String, name :: String }
```

##### Instances
``` purescript
Generic Event _
Eq Event
Show Event
```

#### `Axis`

``` purescript
newtype Axis
  = Axis { description :: String }
```


