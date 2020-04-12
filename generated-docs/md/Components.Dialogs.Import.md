## Module Components.Dialogs.Import

#### `importDialog`

``` purescript
importDialog :: IOQueues Queue ImportDialog (Maybe File) -> ReactElement
```

If the user just decides to close the dialog themselves, then `Nothing` is
returned. Otherwise, it's just the file.

#### `ImportDialog`

``` purescript
data ImportDialog
  = Open
  | Close
  | Failed
```

Externally supplied signals to command the dialog


