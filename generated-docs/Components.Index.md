## Module Components.Index

#### `drawerWidth`

``` purescript
drawerWidth :: Int
```

#### `styles`

``` purescript
styles :: forall t14 t15. { spacing :: { unit :: Number | t14 } | t15 } -> { content :: { flexGrow :: Int, height :: String, marginBottom :: Number, marginTop :: Int, paddingTop :: Number }, contentEditMode :: { flexGrow :: Int, height :: String, marginBottom :: Number, marginLeft :: Int, marginRight :: Int, marginTop :: Int, paddingTop :: Number }, drawer :: { flexShrink :: Int, width :: Int }, drawerPaper :: { height :: String, marginBottom :: Number, marginTop :: Number, width :: Int }, leftDrawerList :: { height :: String, overflowY :: String }, rightDrawerList :: { height :: String, overflowY :: String } }
```

#### `State`

``` purescript
type State = { isEditable :: Boolean }
```

#### `initialState`

``` purescript
initialState :: IxSignal (read :: READ) Settings -> Effect State
```

#### `index`

``` purescript
index :: { logicFunctions :: LogicFunctions, primaryQueues :: PrimaryQueues, primarySignals :: PrimarySignals, stateRef :: Ref Unit } -> ReactElement
```

This component takes the top-level functions, queues, and signals created
in `Main`, and distributes them to its child elements.


