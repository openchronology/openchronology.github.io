## Module Timeline.Data.TimelineName

#### `TimelineName`

``` purescript
newtype TimelineName
  = TimelineName { description :: String, title :: String }
```

Represents both the filename and the timeline's presented name

##### Instances
``` purescript
Generic TimelineName _
Eq TimelineName
Show TimelineName
EncodeJson TimelineName
DecodeJson TimelineName
```

#### `localstorageSignalKey`

``` purescript
localstorageSignalKey :: String
```

#### `localstorageKey`

``` purescript
localstorageKey :: String
```

#### `newTimelineNameSignal`

``` purescript
newTimelineNameSignal :: IxSignal (read :: READ) Settings -> Effect (IxSignal (read :: READ, write :: WRITE) TimelineName)
```

Chosen timeline name on boot, disregarding the shared signal

#### `clearTimelineNameCache`

``` purescript
clearTimelineNameCache :: Effect Unit
```

#### `setDefaultTimelineName`

``` purescript
setDefaultTimelineName :: IxSignal (write :: WRITE) TimelineName -> Effect Unit
```


