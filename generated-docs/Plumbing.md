## Module Plumbing

#### `PrimaryQueues`

``` purescript
type PrimaryQueues = { exportQueue :: Queue (write :: WRITE) ExportDialog, importQueues :: IOQueues Queue ImportDialog (Maybe File), newQueues :: IOQueues Queue Unit Boolean, settingsEditQueues :: IOQueues Queue Unit (Maybe Settings), snackbarQueue :: Queue (write :: WRITE) SnackbarContent, timeScaleEditQueues :: IOQueues Queue Unit (Maybe TimeScale), timelineNameEditQueues :: IOQueues Queue Unit (Maybe TimelineName) }
```

Mostly just Dialog invocations

#### `newPrimaryQueues`

``` purescript
newPrimaryQueues :: Effect PrimaryQueues
```

Created only on boot of the program

#### `PrimarySignals`

``` purescript
type PrimarySignals = { settingsSignal :: IxSignal (read :: READ, write :: WRITE) Settings, timeScaleSignal :: IxSignal (read :: READ, write :: WRITE) TimeScale, timelineNameSignal :: IxSignal (read :: READ, write :: WRITE) TimelineName, zoomSignal :: IxSignal (read :: READ, write :: WRITE) Number }
```

shared state signals

#### `newPrimarySignals`

``` purescript
newPrimarySignals :: Effect PrimarySignals
```

Created only on boot of the program

#### `LogicFunctions`

``` purescript
type LogicFunctions = { onClickedExport :: Effect Unit, onExport :: Effect Unit, onImport :: Effect Unit, onNew :: Effect Unit, onSettingsEdit :: Effect Unit, onTimeScaleEdit :: Effect Unit, onTimelineNameEdit :: Effect Unit }
```

Functions given to the React.js components, to interact with the async devices.
They're the same functions as `Plumbing.Logic`, but with parameters applied.

#### `logic`

``` purescript
logic :: PrimaryQueues -> PrimarySignals -> LogicFunctions
```

Create the logical functions


