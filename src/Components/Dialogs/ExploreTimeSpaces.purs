module Components.Dialogs.ExploreTimeSpaces where

import Timeline.UI.ExploreTimeSpaces
  ( ExploreTimeSpaces
  , ExploreTimeSpacesWithAux(..)
  , updateAux
  , setAuxPreceeding
  , exploreTimeSpacesWithAux
  , updateExploreTimeSpacesWithAux
  )
import Timeline.Time.Bounds (DecidedBounds(..))
import Timeline.Time.Class (asSecondaryString)
import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Array (snoc) as Array
import Data.Array.Indexed (toUnfoldable) as IxArray
import Data.Foldable (foldMap)
import Data.UUID (UUID)
import Data.UUID (parseUUID) as UUID
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import IOQueues (IOQueues(..))
import Queue.One (Queue, put)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , setState
  , getState
  )
import React.DOM (text)
import React.SyntheticEvent (preventDefault, stopPropagation)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.Button (button)
import MaterialUI.IconButton (iconButton)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText')
import MaterialUI.Collapse (collapse)
import MaterialUI.Enums (secondary, auto)
import MaterialUI.Icons.ExpandLessIcon (expandLessIcon)
import MaterialUI.Icons.ExpandMoreIcon (expandMoreIcon)
import Partial.Unsafe (unsafePartial)

type State
  = { open :: Boolean
    , timeSpaces :: ExploreTimeSpacesWithAux Boolean -- Auxiallary data is "open"
    , selected :: Array UUID
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) ExploreTimeSpaces ->
  IxSig.IxSignal ( read :: S.READ ) (Array UUID) ->
  Effect State
initialState exploreTimeSpacesSignal timeSpaceSelectedSignal = do
  timeSpaces <- IxSig.get exploreTimeSpacesSignal
  selected <- IxSig.get timeSpaceSelectedSignal
  pure
    { open: false
    , selected
    , timeSpaces: setAuxPreceeding true selected (exploreTimeSpacesWithAux false timeSpaces)
    }

exploreTimeSpacesDialog ::
  { exploreTimeSpacesSignal :: IxSig.IxSignal ( read :: S.READ ) ExploreTimeSpaces
  , timeSpaceSelectedSignal :: IxSig.IxSignal ( read :: S.READ ) (Array UUID)
  , exploreTimeSpacesQueues :: IOQueues Queue Unit (Maybe (Array UUID))
  } ->
  ReactElement
exploreTimeSpacesDialog { exploreTimeSpacesSignal
, timeSpaceSelectedSignal
, exploreTimeSpacesQueues: IOQueues { input, output }
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "ExploreTimeSpacesDialog" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }

      handlerChangeTimeSpaceSelected :: _ -> Array UUID -> Effect Unit
      handlerChangeTimeSpaceSelected this selected = do
        { timeSpaces } <- getState this
        setState this { selected, timeSpaces: setAuxPreceeding true selected timeSpaces }

      -- FIXME how will residual opened state after timespaces change be affected?
      handlerChangeTimeSpaces :: _ -> ExploreTimeSpaces -> Effect Unit
      handlerChangeTimeSpaces this newTimeSpaces = do
        { timeSpaces } <- getState this
        setState this
          { timeSpaces: updateExploreTimeSpacesWithAux false timeSpaces newTimeSpaces
          }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx exploreTimeSpacesSignal "ExploreTimeSpacesDialog" handlerChangeTimeSpaces
        $ whileMountedIx timeSpaceSelectedSignal "ExploreTimeSpacesDialog" handlerChangeTimeSpaceSelected constructor
    where
    constructor :: ReactClassConstructor _ State _
    constructor this = do
      state <- initialState exploreTimeSpacesSignal timeSpaceSelectedSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  setState this { open: false }
                  put output Nothing

                submit = do
                  { selected } <- getState this
                  setState this { open: false }
                  put output (Just selected)

                toggledOpen path e = do
                  preventDefault e
                  stopPropagation e
                  { timeSpaces } <- getState this
                  setState this { timeSpaces: updateAux not path timeSpaces }

                select path = setState this { selected: path }
              { open, selected, timeSpaces } <- getState this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , fullWidth: true
                    , "aria-labelledby": "exploreTimeSpaces-dialog-title"
                    }
                    [ dialogTitle { id: "exploreTimeSpaces-dialog-title" } [ text "Explore TimeSpaces" ]
                    , dialogContent_
                        [ let
                            printListItem :: Array UUID -> ExploreTimeSpacesWithAux Boolean -> Int -> Array ReactElement
                            printListItem path (ExploreTimeSpacesWithAux x) paddingLeft =
                              [ listItem
                                  { button: true
                                  , onClick: mkEffectFn1 (const (select path))
                                  , selected: path == selected
                                  , style: { paddingLeft }
                                  }
                                  $ [ listItemText'
                                        { inset: true
                                        , primary: x.name
                                        , secondary: asSecondaryString x.bounds
                                        }
                                    ]
                                  <> case x.children of
                                      Nothing -> []
                                      Just { aux: open' } ->
                                        [ iconButton { onClick: mkEffectFn1 (toggledOpen path) }
                                            [ if open' then expandLessIcon else expandMoreIcon ]
                                        ]
                              ]
                                <> case x.children of
                                    Nothing -> []
                                    Just { aux: open', childrenValues } ->
                                      [ collapse { "in": open', timeout: auto, unmountOnExit: true }
                                          [ list { disablePadding: true }
                                              $ let
                                                  xs :: Array _
                                                  xs = IxArray.toUnfoldable childrenValues

                                                  go (Tuple index nextBranch) =
                                                    let
                                                      index' = unsafePartial $ fromJust $ UUID.parseUUID index
                                                    in
                                                      printListItem (Array.snoc path index') nextBranch (paddingLeft + 8)
                                                in
                                                  foldMap go xs
                                          ]
                                      ]
                          in
                            list { disablePadding: true }
                              $ printListItem [] timeSpaces 16
                        ]
                    , dialogActions_
                        [ button { onClick: mkEffectFn1 (const close) } [ text "Cancel" ]
                        , button { onClick: mkEffectFn1 (const submit), color: secondary, autoFocus: true } [ text "Submit" ]
                        ]
                    ]
        }
