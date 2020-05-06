## Module Timeline.Data.Event

#### `Event`

``` purescript
newtype Event a
  = Event { description :: String, index :: a, name :: String }
```

An event documented at time `index`.

Defined over the user-level timescale `a`.


