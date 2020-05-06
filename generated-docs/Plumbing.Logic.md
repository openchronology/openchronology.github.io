## Module Plumbing.Logic

#### `onImport`

``` purescript
onImport :: { importQueues :: IOQueues Queue ImportDialog (Maybe File), settingsSignal :: IxSignal (read :: READ, write :: WRITE) Settings, timelineNameSignal :: IxSignal (read :: READ) TimelineName } -> Effect Unit
```

When the "Import" button is clicked

#### `onExport`

``` purescript
onExport :: { exportQueue :: Queue (write :: WRITE) ExportDialog } -> Effect Unit
```

When opening the "Export" window

#### `onClickedExport`

``` purescript
onClickedExport :: { snackbarQueue :: Queue (write :: WRITE) SnackbarContent } -> Effect Unit
```

Clears local unsaved cache, and triggers a snackbar message

#### `onNew`

``` purescript
onNew :: { newQueues :: IOQueues Queue Unit Boolean, timeScaleSignal :: IxSignal (write :: WRITE) TimeScale, timelineNameSignal :: IxSignal (write :: WRITE) TimelineName } -> Effect Unit
```

open new timeline

#### `onTimelineNameEdit`

``` purescript
onTimelineNameEdit :: { timelineNameEditQueues :: IOQueues Queue Unit (Maybe TimelineName), timelineNameSignal :: IxSignal (read :: READ, write :: WRITE) TimelineName } -> Effect Unit
```

Invokes dialog queues and stores the result in state signals

#### `onTimeScaleEdit`

``` purescript
onTimeScaleEdit :: { timeScaleEditQueues :: IOQueues Queue Unit (Maybe TimeScale), timeScaleSignal :: IxSignal (read :: READ, write :: WRITE) TimeScale } -> Effect Unit
```

#### `onSettingsEdit`

``` purescript
onSettingsEdit :: { settingsEditQueues :: IOQueues Queue Unit (Maybe Settings), settingsSignal :: IxSignal (read :: READ, write :: WRITE) Settings } -> Effect Unit
```


