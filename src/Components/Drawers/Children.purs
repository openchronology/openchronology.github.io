module Components.Drawers.Children where

import Timeline.UI.Event (Event(..))
import Timeline.UI.TimeSpan (TimeSpan(..))
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan)
import Timeline.UI.EventOrTimeSpans (EventOrTimeSpans)
import Timeline.UI.Index.Class (asSecondaryString)
import Timeline.UI.Index.Value (DecidedValue(..))
import Timeline.UI.Index.Span (DecidedSpan(..))
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
  = { elements :: EventOrTimeSpans
    , selected :: Maybe Int
    , menuAnchor :: Maybe NativeEventTarget
    , isEditable :: Boolean
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState settingsSignal = do
  Settings { isEditable } <- IxSig.get settingsSignal
  pure
    { elements:
        [ Left (Event { name: "Event A", description: "baz", time: DecidedValueNumber 3.0 })
        , Left (Event { name: "Event B", description: "qux", time: DecidedValueNumber 3.5 })
        , Right (TimeSpan { name: "TimeSpan C", description: "bar", span: DecidedSpanNumber { start: 2.0, stop: 5.0 }, timeSpace: Nothing })
        , Right (TimeSpan { name: "TimeSpan D", description: "foo", span: DecidedSpanNumber { start: 1.0, stop: 4.0 }, timeSpace: Nothing })
        ]
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
  } ->
  ReactElement
childrenDrawer { settingsSignal } = createLeafElement c {}
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
    in
      whileMountedIx settingsSignal "ChildrenDrawer" handleChangeEdit constructor'
    where
    constructor' this = do
      state <- initialState settingsSignal
      pure
        { state
        , componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , render:
            do
              props <- getProps this
              { elements, selected, menuAnchor, isEditable } <- getState this
              let
                handleMenuClick e = do
                  anchor <- currentTarget e
                  setState this { menuAnchor: Just anchor }

                handleClose = setState this { menuAnchor: Nothing }

                mkTextItemTime :: Int -> EventOrTimeSpan -> ReactElement
                mkTextItemTime i eOrT =
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
