## Module Components.BottomBar

#### `bottomBar`

``` purescript
bottomBar :: { onTimeScaleEdit :: Effect Unit, timeScaleSignal :: IxSignal (read :: READ) TimeScale, zoomSignal :: IxSignal (read :: READ) Number } -> ReactElement
```

The signals give some state to this component, while the functions are
how the component interact with the queues.


