module Components.Drawers.Siblings where

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
import React.SyntheticEvent (currentTarget, NativeEventTarget)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.List (list_, list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText')
import MaterialUI.ListItemSecondaryAction (listItemSecondaryAction_)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Icons.MoreHorizIcon (moreHorizIcon)
import MaterialUI.Button (button)
import MaterialUI.Menu (menu)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Enums (title, subheading, permanent, right, small, raised, primary)



type State =
  { elements :: Array {name :: String, time :: String} -- FIXME time-sorted mapping?
  , selected :: Maybe Int
  , menuAnchor :: Maybe NativeEventTarget
  }


initialState :: Effect State
initialState = pure
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
  }



styles :: _
styles theme =
  { leftDrawerList:
    { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 3.0) + (30.75 * 2.0)) <> "px)"
    , overflowY: "auto"
    }
  }


siblingsDrawer :: ReactElement
siblingsDrawer = createLeafElement c {}
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
    constructor = constructor'
      where
        constructor' this = do
          state <- initialState
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
              props <- getProps this
              {elements,selected,menuAnchor} <- getState this
              let handleMenuClick e = do
                    anchor <- currentTarget e
                    setState this {menuAnchor: Just anchor}
                  handleClose = setState this {menuAnchor: Nothing}
                  mkTextItemTime i {name,time} =
                    listItem
                      { button: true
                      , selected: isSelected
                      , onClick: mkEffectFn1 (const select)
                      }
                      [ listItemText' {primary: name, secondary: time}
                      , listItemSecondaryAction_
                        [ iconButton
                          { onClick: mkEffectFn1 handleMenuClick
                          }
                          [moreHorizIcon]
                        ]
                      ]
                    where
                      isSelected = Just i == selected
                      select = setState this {selected: if isSelected then Nothing else Just i}
              pure $ toElement
                [ typography {variant: title} [text "Events and TimeSpans"]
                , typography {variant: subheading} [text "For Multiple Timelines"]
                , button {size: small, variant: raised, color: primary} [text "Add"]
                , list {className: props.classes.leftDrawerList} (Array.mapWithIndex mkTextItemTime elements)
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
