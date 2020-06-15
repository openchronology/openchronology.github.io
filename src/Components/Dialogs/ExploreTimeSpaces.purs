module Components.Dialogs.ExploreTimeSpaces where

import Timeline.UI.ExploreTimeSpaces
  ( WithSpanOfTime(..)
  , ExploreTimeSpaces
  , ExploreTimeSpacesWithAux(..)
  , toggleOpen
  , openThrough
  , exploreTimeSpacesWithAux
  , updateExploreTimeSpacesWithAux
  )
import Timeline.Data.TimeComponent (SpanOfTime(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.IxSet.Demi (Index)
import Data.IxSet.Demi (toUnfoldable) as IxDemiSet
import Data.Array (snoc) as Array
import Data.Foldable (foldMap)
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

type State
  = { open :: Boolean
    , timeSpaces :: WithSpanOfTime ExploreTimeSpacesWithAux
    , selected :: Array Index
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) (WithSpanOfTime ExploreTimeSpaces) ->
  IxSig.IxSignal ( read :: S.READ ) (Array Index) ->
  Effect State
initialState exploreTimeSpacesSignal timeSpaceSelectedSignal = do
  timeSpaces <- IxSig.get exploreTimeSpacesSignal
  selected <- IxSig.get timeSpaceSelectedSignal
  pure
    { open: false
    , selected
    , timeSpaces: (openThrough selected <<< exploreTimeSpacesWithAux) <$> timeSpaces
    }

exploreTimeSpacesDialog ::
  { exploreTimeSpacesSignal :: IxSig.IxSignal ( read :: S.READ ) (WithSpanOfTime ExploreTimeSpaces)
  , timeSpaceSelectedSignal :: IxSig.IxSignal ( read :: S.READ ) (Array Index)
  , exploreTimeSpacesQueues :: IOQueues Queue Unit (Maybe (Array Index))
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

      handlerChangeTimeSpaceSelected :: _ -> Array Index -> Effect Unit
      handlerChangeTimeSpaceSelected this selected = do
        { timeSpaces } <- getState this
        setState this { selected, timeSpaces: openThrough selected <$> timeSpaces }

      -- FIXME how will residual opened state after timespaces change be affected?
      handlerChangeTimeSpaces :: _ -> WithSpanOfTime ExploreTimeSpaces -> Effect Unit
      handlerChangeTimeSpaces this newTimeSpaces = do
        { timeSpaces } <- getState this
        setState this
          { timeSpaces: updateExploreTimeSpacesWithAux <$> timeSpaces <*> newTimeSpaces
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
                  setState this { timeSpaces: toggleOpen path <$> timeSpaces }

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
                            printListItem :: Array Index -> SpanOfTime String -> ExploreTimeSpacesWithAux -> Int -> Array ReactElement
                            printListItem path (SpanOfTime t) (ExploreTimeSpacesWithAux x) paddingLeft =
                              [ listItem
                                  { button: true
                                  , onClick: mkEffectFn1 (const (select path))
                                  , selected: path == selected
                                  , style: { paddingLeft }
                                  }
                                  $ [ listItemText'
                                        { inset: true
                                        , primary: x.name
                                        , secondary: "From: " <> t.startTime <> ", To: " <> t.stopTime
                                        }
                                    ]
                                  <> case x.children of
                                      Nothing -> []
                                      Just { open: open' } ->
                                        [ iconButton { onClick: mkEffectFn1 (toggledOpen path) }
                                            [ if open' then expandLessIcon else expandMoreIcon ]
                                        ]
                              ]
                                <> case x.children of
                                    Nothing -> []
                                    Just { open: open', children } ->
                                      [ collapse { "in": open', timeout: auto, unmountOnExit: true }
                                          [ list { disablePadding: true }
                                              $ let
                                                  xs :: Array _
                                                  xs = IxDemiSet.toUnfoldable children

                                                  go { index, key, value } = printListItem (Array.snoc path index) key value (paddingLeft + 8)
                                                in
                                                  foldMap go xs
                                          ]
                                      ]
                          in
                            case timeSpaces of
                              WithSpanOfTime x ->
                                list { disablePadding: true }
                                  $ printListItem [] x.spanOfTime x.timeSpaces 16
                        ]
                    , dialogActions_
                        [ button { onClick: mkEffectFn1 (const close) } [ text "Cancel" ]
                        , button { onClick: mkEffectFn1 (const submit), color: secondary, autoFocus: true } [ text "Submit" ]
                        ]
                    ]
        }
