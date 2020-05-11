module Components.Drawers.Siblings where

import Settings (Settings (..))

import Prelude
import Data.Maybe (Maybe (..), isJust)
import Data.Array (mapWithIndex) as Array
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , toElement, component, setState, getState, getProps, createLeafElement)
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
import MaterialUI.Enums (title, subheading, small, raised, primary)
import MaterialUI.Theme (Theme)
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig



type State =
  { elements :: Array {name :: String, time :: String} -- FIXME time-sorted mapping?
  , selected :: Maybe Int
  , menuAnchor :: Maybe NativeEventTarget
  , isEditable :: Boolean
  }


initialState :: IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState settingsSignal = do
  Settings {isEditable} <- IxSig.get settingsSignal
  pure
    { elements:
      [ {name: "Event A", time: "20200130"}
      , {name: "Event B", time: "20200131"}
      , {name: "TimeSpan C", time: "20200202"}
      , {name: "TimeSpan D", time: "20200204"}
      , {name: "Event E", time: "20200208"}
      , {name: "Event F", time: "20200211"}
      ]
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


siblingsDrawer :: { settingsSignal :: IxSig.IxSignal (read :: S.READ) Settings
                  } -> ReactElement
siblingsDrawer {settingsSignal} = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        c' :: ReactClass
              { classes ::
                { leftDrawerList :: String
                }
              }
        c' = component "SiblingsDrawer" constructor

    constructor :: ReactClassConstructor _ State _
    constructor =
      let handleChangeEdit this (Settings {isEditable}) = setState this {isEditable}
      in  whileMountedIx settingsSignal "SiblingsDrawer" handleChangeEdit constructor'
      where
        constructor' this = do
          state <- initialState settingsSignal
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
              props <- getProps this
              {elements,selected,menuAnchor,isEditable} <- getState this
              let handleMenuClick e = do
                    anchor <- currentTarget e
                    setState this {menuAnchor: Just anchor}
                  handleClose = setState this {menuAnchor: Nothing}
                  mkTextItemTime i {name,time} =
                    listItem
                      { button: true
                      , selected: isSelected
                      , onClick: mkEffectFn1 (const select)
                      } $
                      [ listItemText' {primary: name, secondary: time}
                      ] <>
                        ( if isEditable
                            then
                              [ listItemSecondaryAction_
                                [ iconButton
                                  { onClick: mkEffectFn1 handleMenuClick
                                  }
                                  [moreHorizIcon]
                                ]
                              ]
                            else []
                        )
                    where
                      isSelected = Just i == selected
                      select = setState this {selected: if isSelected then Nothing else Just i}
              pure $ toElement $
                [ typography {variant: title} [text "Events and TimeSpans"]
                , typography {variant: subheading} [text "For Multiple Timelines"]
                ] <>
                  ( if isEditable
                      then [button {size: small, variant: raised, color: primary} [text "Add"]]
                      else []
                  ) <>
                [ list {className: props.classes.leftDrawerList} (Array.mapWithIndex mkTextItemTime elements)
                , menu
                  { id: "siblings-item-menu"
                  , anchorEl: toNullable menuAnchor
                  , open: isJust menuAnchor
                  , onClose: mkEffectFn1 (const handleClose)
                  }
                  [ menuItem
                    { onClick: mkEffectFn1 (const handleClose)
                    } [text "Edit"]
                  , menuItem
                    { onClick: mkEffectFn1 (const handleClose)
                    } [text "Move"]
                  , menuItem
                    { onClick: mkEffectFn1 (const handleClose)
                    } [text "Delete"]
                  ]
                ]
            }
