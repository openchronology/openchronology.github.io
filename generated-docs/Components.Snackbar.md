## Module Components.Snackbar

#### `snackbars`

``` purescript
snackbars :: Queue (read :: READ) SnackbarContent -> ReactElement
```

Shows all the active snackbars, and pulls them from the Queue.

#### `SnackbarVariant`

``` purescript
data SnackbarVariant
  = Success
  | Error
  | Info
  | Warning
```

The type of snackbar being sent

#### `SnackbarContent`

``` purescript
type SnackbarContent = { message :: String, timeout :: Maybe Milliseconds, variant :: SnackbarVariant }
```

All the information necessary to send a snackbar. Note that if the timeout is Nothing,
the user will have to close the message to get rid of it.


