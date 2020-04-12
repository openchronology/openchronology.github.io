## Module Components.Index

#### `index`

``` purescript
index :: { logicFunctions :: LogicFunctions, primaryQueues :: PrimaryQueues, primarySignals :: PrimarySignals, stateRef :: Ref Unit } -> ReactElement
```

This component takes the top-level functions, queues, and signals created
in `Main`, and distributes them to its child elements.


