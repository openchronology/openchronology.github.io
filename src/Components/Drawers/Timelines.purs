module Components.Drawers.Timelines where

import Timeline.UI.Timeline (Timeline (..))
import Timeline.UI.Timelines (Timelines (..))
import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Array (mapWithIndex) as Array
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , toElement
  , component
  , setState
  , getState
  , getProps
  , createLeafElement
  )
import React.DOM (text)
import React.Signal.WhileMounted (whileMountedIx)
import React.SyntheticEvent (currentTarget, NativeEventTarget)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText')
import MaterialUI.ListItemSecondaryAction (listItemSecondaryAction_)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Icons.MoreHorizIcon (moreHorizIcon)
import MaterialUI.Button (button)
import MaterialUI.Menu (menu)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Enums (title, small, contained, primary)
import MaterialUI.Theme (Theme)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Partial.Unsafe (unsafePartial)





type State
  = { elements :: Timelines
    , selected :: Maybe Int
    , menuAnchor :: Maybe
      { target :: NativeEventTarget
      , index :: Int
      }
    , isEditable :: Boolean
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  IxSig.IxSignal (read :: S.READ) Timelines ->
  Effect State
initialState settingsSignal timelinesSignal = do
  Settings { isEditable } <- IxSig.get settingsSignal
  timelines <- IxSig.get timelinesSignal
  pure
    { elements: timelines
    , selected: Nothing
    , menuAnchor: Nothing
    , isEditable
    }

styles :: Theme -> _
styles theme =
  { leftDrawerList:
      { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 3.0) + (30.75 * 2.0)) <> "px)"
      , overflowY: "auto"
      }
  }

timelinesDrawer ::
  { settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , timelinesSignal :: IxSig.IxSignal (read :: S.READ) Timelines
  , onClickedNewTimeline :: Effect Unit
  , onClickedEditTimeline :: Int -> Effect Unit
  , onClickedDeleteTimeline :: Int -> Effect Unit
  } ->
  ReactElement
timelinesDrawer
  { settingsSignal
  , timelinesSignal
  , onClickedNewTimeline
  , onClickedEditTimeline
  , onClickedDeleteTimeline
  } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' ::
      ReactClass
        { classes ::
            { leftDrawerList :: String
            }
        }
    c' = component "TimelinesDrawer" constructor

  constructor :: ReactClassConstructor _ State _
  constructor =
    let
      handleChangeEdit this (Settings { isEditable }) = setState this { isEditable }
      handleChangeTimelines this ts = setState this { elements: ts }
    in
      whileMountedIx settingsSignal "TimelinesDrawer" handleChangeEdit $
      whileMountedIx timelinesSignal "TimelinesDrawer" handleChangeTimelines constructor'
    where
    constructor' this = do
      state <- initialState settingsSignal timelinesSignal
      pure
        { state
        , componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , render:
            do
              props <- getProps this
              { elements: Timelines elements, selected, menuAnchor, isEditable } <- getState this
              let
                handleMenuClick :: Int -> _ -> Effect Unit
                handleMenuClick i e = do
                  anchor <- currentTarget e
                  setState this { menuAnchor: Just {target: anchor, index: i} }

                handleClose = setState this { menuAnchor: Nothing }

                mkTextItem :: Int -> Timeline -> ReactElement
                mkTextItem i (Timeline {name}) =
                  listItem
                    { button: true
                    , selected: isSelected
                    , onClick: mkEffectFn1 (const select)
                    }
                    $ [ listItemText' { primary: name }
                      ]
                    <> ( if isEditable then
                          [ listItemSecondaryAction_
                              [ iconButton
                                  { onClick: mkEffectFn1 (handleMenuClick i)
                                  }
                                  [ moreHorizIcon ]
                              ]
                          ]
                        else
                          []
                      )
                  where
                  isSelected = Just i == selected

                  select = setState this { selected: if isSelected then Nothing else Just i }
              pure $ toElement
                $ [ typography { variant: title } [ text "Timelines" ]
                  ]
                <> ( if isEditable then
                      [ button
                        { size: small
                        , variant: contained
                        , onClick: mkEffectFn1 (const onClickedNewTimeline)
                        } [ text "Add" ]
                      ]
                    else
                      []
                  )
                <> [ list { className: props.classes.leftDrawerList } (Array.mapWithIndex mkTextItem elements)
                  , menu
                      { id: "timelines-menu"
                      , anchorEl: toNullable (map _.target menuAnchor)
                      , open: isJust menuAnchor
                      , onClose: mkEffectFn1 (const handleClose)
                      }
                      [ menuItem
                          { onClick: mkEffectFn1 \_ -> do
                              handleClose
                              {menuAnchor} <- getState this
                              unsafePartial $ case menuAnchor of
                                Just {index} ->
                                  onClickedEditTimeline index
                          }
                          [ text "Edit" ]
                      , menuItem
                          { onClick: mkEffectFn1 \_ -> do
                              handleClose
                              -- TODO timespace explorer
                          }
                          [ text "Move" ]
                      , menuItem
                          { onClick: mkEffectFn1 \_ -> do
                              handleClose
                              {menuAnchor} <- getState this
                              unsafePartial $ case menuAnchor of
                                Just {index} ->
                                  onClickedDeleteTimeline index
                          }
                          [ text "Delete" ]
                      ]
                  ]
        }
