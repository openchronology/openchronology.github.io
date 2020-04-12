## Module Main

#### `mountToRoot`

``` purescript
mountToRoot :: ReactElement -> Effect (Maybe ReactComponent)
```

Get the `Document` node, and look for the `<div id='root'></div>` element
(found in the templates - see [build/README.md](../../build/README.md)).

#### `main`

``` purescript
main :: Effect Unit
```

Start application


