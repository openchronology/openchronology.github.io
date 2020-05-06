## Module Timeline.Data.TimeSpan

#### `TimeSpan`

``` purescript
newtype TimeSpan a
  = TimeSpan { description :: String, name :: String, startIndex :: a, stopIndex :: a }
```

An inclusive span of time from `startIndex` to `stopIndex`.

Defined over the user-level timescale `a`.


