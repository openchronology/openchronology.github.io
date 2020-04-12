## Module Settings

#### `Settings`

``` purescript
newtype Settings
  = Settings { isEditable :: Boolean, localCacheTilExport :: Boolean }
```

The `Settings` record, which is JSON encodable so it can be stored.

##### Instances
``` purescript
Generic Settings _
Eq Settings
Show Settings
EncodeJson Settings
DecodeJson Settings
```

#### `localstorageSignalKey`

``` purescript
localstorageSignalKey :: String
```

The key to be used by the `Handler` that listens to the signal for changes

#### `localstorageKey`

``` purescript
localstorageKey :: String
```

The key to be used in LocalStorage when storing or looking up data

#### `newSettingsSignal`

``` purescript
newSettingsSignal :: { wasOpenedByShareLink :: Boolean } -> Effect (IxSignal (read :: READ, write :: WRITE) Settings)
```

Create a `Signal` which updates the LocalStorage record whenever the settings change.
The initial argument makes sure the interface isn't in edit mode when opening a shared link.

#### `defaultSettings`

``` purescript
defaultSettings :: Settings
```

What the settings value should be for new users, with no link opened.


