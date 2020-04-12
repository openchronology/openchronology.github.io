## Module Components.Dialogs.Export

#### `exportDialog`

``` purescript
exportDialog :: { exportQueue :: Queue (read :: READ) ExportDialog, onClickedExport :: Effect Unit } -> ReactElement
```

When "Export" is clicked, the app knows to delete cached / "unsaved" data from LocalStorage,
because... now it's clearly had every opportunity to be saved.

#### `ExportDialog`

``` purescript
newtype ExportDialog
  = ExportDialog { buffer :: ArrayBuffer, filename :: String }
```

The content of the dialog


