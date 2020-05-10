module Components.Drawers.Timelines where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array (mapWithIndex) as Array
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , toElement, component, setState, getState, getProps, createLeafElement)
import React.DOM (text)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.List (list_, list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText')
import MaterialUI.Enums (title, subheading, permanent, right)



type State =
  { elements :: Array String -- FIXME time-sorted mapping?
  , selected :: Maybe Int
  }


initialState :: Effect State
initialState = pure
  { elements:
    [ "Timeline A"
    , "Timeline B"
    , "Timeline C"
    , "Timeline D"
    , "Timeline E"
    , "Timeline F"
    , "Timeline G"
    ]
  , selected: Nothing
  }



styles :: _
styles theme =
  { leftDrawerList:
    { height: "calc(100vh - " <> show ((theme.spacing.unit * 12.0) + (24.5 * 3.0)) <> "px)"
    , overflowY: "auto"
    }
  }


timelinesDrawer :: ReactElement
timelinesDrawer = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        c' :: ReactClass
              { classes ::
                { leftDrawerList :: String
                }
              }
        c' = component "TimelinesDrawer" constructor

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
              {elements,selected} <- getState this
              let mkTextItem i t =
                    listItem
                      { button: true
                      , selected: isSelected
                      , onClick: mkEffectFn1 (const select)
                      } [listItemText' {primary: t}]
                    where
                      isSelected = Just i == selected
                      select = setState this {selected: if isSelected then Nothing else Just i}
              pure $ toElement
                [ typography {variant: title} [text "Timelines"]
                , list {className: props.classes.leftDrawerList} (Array.mapWithIndex mkTextItem elements)
                ]
            }
