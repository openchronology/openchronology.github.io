## Module Timeline.Data.TimeScale

#### `TimeScale`

``` purescript
newtype TimeScale
  = TimeScale { description :: String, name :: String, units :: String }
```

##### Instances
``` purescript
Generic TimeScale _
Eq TimeScale
Show TimeScale
EncodeJson TimeScale
DecodeJson TimeScale
```

#### `localstorageSignalKey`

``` purescript
localstorageSignalKey :: String
```

#### `localstorageKey`

``` purescript
localstorageKey :: String
```

#### `newTimeScaleSignal`

``` purescript
newTimeScaleSignal :: IxSignal (read :: READ) Settings -> Effect (IxSignal (read :: READ, write :: WRITE) TimeScale)
```

#### `clearTimeScaleCache`

``` purescript
clearTimeScaleCache :: Effect Unit
```

#### `setDefaultTimeScale`

``` purescript
setDefaultTimeScale :: IxSignal (write :: WRITE) TimeScale -> Effect Unit
```


