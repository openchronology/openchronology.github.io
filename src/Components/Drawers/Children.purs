module Components.Drawers.Children where

import Timeline.UI.Event (Event(..))
import Timeline.UI.TimeSpan (TimeSpan(..))
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan(..))
import Timeline.UI.Children (Children(..))
import Timeline.UI.Index.Class (asSecondaryString)
import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Either (Either(..))
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
import MaterialUI.Enums (h6, subtitle1, small, contained)
import MaterialUI.Theme (Theme)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Partial.Unsafe (unsafePartial)

type State
  = { elements :: Children -- Array { name :: String, time :: String } -- FIXME time-sorted mapping?
    , selected :: Maybe Int
    , menuAnchor ::
        Maybe
          { target :: NativeEventTarget
          , index :: Int
          }
    , isEditable :: Boolean
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  IxSig.IxSignal ( read :: S.READ ) Children ->
  Effect State
initialState settingsSignal childrenSignal = do
  Settings { isEditable } <- IxSig.get settingsSignal
  elements <- IxSig.get childrenSignal
  pure
    { elements
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

childrenDrawer ::
  { settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , childrenSignal :: IxSig.IxSignal ( read :: S.READ ) Children
  , onClickedNewEventOrTimeSpanChildren :: Effect Unit
  , onClickedEditEventOrTimeSpanChildren :: Int -> Effect Unit
  , onClickedDeleteEventOrTimeSpanChildren :: Int -> Effect Unit
  } ->
  ReactElement
childrenDrawer { settingsSignal
, childrenSignal
, onClickedNewEventOrTimeSpanChildren
, onClickedEditEventOrTimeSpanChildren
, onClickedDeleteEventOrTimeSpanChildren
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
    c' = component "ChildrenDrawer" constructor

  constructor :: ReactClassConstructor _ State _
  constructor =
    let
      handleChangeEdit this (Settings { isEditable }) = setState this { isEditable }

      handleChangeChildren this elements = setState this { elements }
    in
      whileMountedIx settingsSignal "ChildrenDrawer" handleChangeEdit
        $ whileMountedIx childrenSignal "ChildrenDrawer" handleChangeChildren constructor'
    where
    constructor' this = do
      state <- initialState settingsSignal childrenSignal
      pure
        { state
        , componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , render:
            do
              props <- getProps this
              { elements: Children elements, selected, menuAnchor, isEditable } <- getState this
              let
                handleMenuClick :: Int -> _ -> Effect Unit
                handleMenuClick i e = do
                  anchor <- currentTarget e
                  setState this { menuAnchor: Just { target: anchor, index: i } }

                handleClose = setState this { menuAnchor: Nothing }

                mkTextItemTime :: Int -> EventOrTimeSpan -> ReactElement
                mkTextItemTime i (EventOrTimeSpan eOrT) =
                  let
                    item edit = case eOrT of
                      Left (Event { name, time }) ->
                        listItem
                          { button: true
                          , selected: isSelected
                          , onClick: mkEffectFn1 (const select)
                          , title: "Select Event"
                          }
                          $ [ listItemText' { primary: name, secondary: asSecondaryString time }
                            ]
                          <> edit
                      Right (TimeSpan { name, span }) ->
                        listItem
                          { button: true
                          , selected: isSelected
                          , onClick: mkEffectFn1 (const select)
                          , title: "Select TimeSpan"
                          }
                          $ [ listItemText' { primary: name, secondary: asSecondaryString span }
                            ]
                          <> edit
                  in
                    item
                      [ listItemSecondaryAction_
                          [ iconButton
                              { onClick: mkEffectFn1 (handleMenuClick i)
                              }
                              [ moreHorizIcon ]
                          ]
                      ]
                  where
                  isSelected = Just i == selected

                  select = setState this { selected: if isSelected then Nothing else Just i }
              pure $ toElement
                $ [ typography { variant: h6 } [ text "Events and TimeSpans" ]
                  , typography { variant: subtitle1 } [ text "For Multiple Timelines" ]
                  ]
                <> ( if isEditable then
                      [ button
                          { size: small
                          , variant: contained
                          , onClick: mkEffectFn1 (const onClickedNewEventOrTimeSpanChildren)
                          }
                          [ text "Add" ]
                      ]
                    else
                      []
                  )
                <> [ list { className: props.classes.leftDrawerList } (Array.mapWithIndex mkTextItemTime elements)
                  , menu
                      { id: "children-item-menu"
                      , anchorEl: toNullable (map _.target menuAnchor)
                      , open: isJust menuAnchor
                      , onClose: mkEffectFn1 (const handleClose)
                      }
                      $ if isEditable then
                          [ menuItem
                              { onClick:
                                  mkEffectFn1 \_ -> do
                                    handleClose
                                    { menuAnchor: anchor' } <- getState this
                                    unsafePartial
                                      $ case anchor' of
                                          Just { index } -> onClickedEditEventOrTimeSpanChildren index
                              }
                              [ text "Edit" ]
                          , menuItem
                              { onClick: mkEffectFn1 (const handleClose)
                              -- TODO timespace explorer
                              }
                              [ text "Move" ]
                          , menuItem
                              { onClick:
                                  mkEffectFn1 \_ -> do
                                    handleClose
                                    { menuAnchor: anchor' } <- getState this
                                    unsafePartial
                                      $ case anchor' of
                                          Just { index } -> onClickedDeleteEventOrTimeSpanChildren index
                              }
                              [ text "Delete" ]
                          ]
                        else
                          [ menuItem
                              { onClick:
                                  mkEffectFn1 \_ -> do
                                    handleClose
                                    { menuAnchor: anchor' } <- getState this
                                    unsafePartial
                                      $ case anchor' of
                                          Just { index } -> onClickedEditEventOrTimeSpanChildren index
                              }
                              [ text "Details" ]
                          ]
                  ]
        }
