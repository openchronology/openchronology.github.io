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
import MaterialUI.Enums (title, subheading, small, contained)
import MaterialUI.Theme (Theme)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig

type State
  = { elements :: Children
    , selected :: Maybe Int
    , menuAnchor :: Maybe NativeEventTarget
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
  { rightDrawerList:
      { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 2.0) + 30.75) <> "px)"
      , overflowY: "auto"
      }
  }

childrenDrawer ::
  { settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , childrenSignal :: IxSig.IxSignal ( read :: S.READ ) Children
  } ->
  ReactElement
childrenDrawer { settingsSignal, childrenSignal } = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' ::
      ReactClass
        { classes ::
            { rightDrawerList :: String
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
                handleMenuClick e = do
                  anchor <- currentTarget e
                  setState this { menuAnchor: Just anchor }

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
                          }
                          $ [ listItemText' { primary: name, secondary: asSecondaryString time }
                            ]
                          <> edit
                      Right (TimeSpan { name, span }) ->
                        listItem
                          { button: true
                          , selected: isSelected
                          , onClick: mkEffectFn1 (const select)
                          }
                          $ [ listItemText' { primary: name, secondary: asSecondaryString span }
                            ]
                          <> edit
                  in
                    item
                      ( if isEditable then
                          [ listItemSecondaryAction_
                              [ iconButton
                                  { onClick: mkEffectFn1 handleMenuClick
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
                $ [ typography { variant: title } [ text "Events and TimeSpans" ]
                  , typography { variant: subheading } [ text "For Timeline \"A\"" ]
                  ]
                <> ( if isEditable then
                      [ button { size: small, variant: contained } [ text "Add" ] ]
                    else
                      []
                  )
                <> [ list { className: props.classes.rightDrawerList } (Array.mapWithIndex mkTextItemTime elements)
                  , menu
                      { id: "children-item-menu"
                      , anchorEl: toNullable menuAnchor
                      , open: isJust menuAnchor
                      , onClose: mkEffectFn1 (const handleClose)
                      }
                      [ menuItem
                          { onClick: mkEffectFn1 (const handleClose)
                          }
                          [ text "Edit" ]
                      , menuItem
                          { onClick: mkEffectFn1 (const handleClose)
                          }
                          [ text "Move" ]
                      , menuItem
                          { onClick: mkEffectFn1 (const handleClose)
                          }
                          [ text "Delete" ]
                      ]
                  ]
        }
