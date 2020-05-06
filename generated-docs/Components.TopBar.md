## Module Components.TopBar

#### `topBar`

``` purescript
topBar :: { onExport :: Effect Unit, onImport :: Effect Unit, onNew :: Effect Unit, onSettingsEdit :: Effect Unit, onTimelineNameEdit :: Effect Unit, settingsSignal :: IxSignal (read :: READ) Settings, timelineNameSignal :: IxSignal (read :: READ) TimelineName } -> ReactElement
```

The signals give some state to this component, while the functions are
how the component interact with the queues.


