module Components.BottomBar (bottomBar) where


{-|

Both TopBar and BottomBar are Material-UI "AppBar"s - giving site-wide functionality, like navigation
and access to control the side-wide settings.

-}


import Timeline.Data.TimeScale (TimeScale (..))

import Prelude hiding (div)
import Data.Fixed (fromNumber, toString, P100, Fixed) as Fixed
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Effect.Console (log)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getProps, getState, setState, createLeafElement)
import React.SyntheticEvent (preventDefault, stopPropagation, deltaY)
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import React.DOM.NonBlockingSpace (nbsp)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button, button'')
import MaterialUI.IconButton (iconButton)
import MaterialUI.Icons.AddIcon (addIcon)
import MaterialUI.Icons.RemoveIcon (removeIcon)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (subheading, absolute, inherit, dense)
import Zeta.Types (READ, WRITE, readOnly) as S
import IxZeta (IxSignal, get, set) as IxSig
import Debug.Trace (traceM)



 -- | This CSS allows the app bar to stretch across the screen
styles :: _
styles theme =
  { root:
    { flexGrow: 1
    , top: "auto"
    , bottom: 0
    }
  , center:
    { flexGrow: 1
    , textAlign: "center"
    }
  }

-- | Updates state when either signal is written to
type State =
  { zoom  :: Number
  , name  :: String
  , units :: String
  }

initialState :: IxSig.IxSignal (read :: S.READ) Number
             -> IxSig.IxSignal (read :: S.READ) TimeScale
             -> Effect State
initialState zoomSignal timeScaleSignal = do
  zoom <- IxSig.get zoomSignal
  TimeScale {name, units} <- IxSig.get timeScaleSignal
  pure
    { zoom
    , name
    , units
    }


-- | The signals give some state to this component, while the functions are
-- | how the component interact with the queues.
bottomBar :: { onTimeScaleEdit :: Effect Unit
             , zoomSignal :: IxSig.IxSignal (read :: S.READ, write :: S.WRITE) Number
             , timeScaleSignal :: IxSig.IxSignal (read :: S.READ) TimeScale
             } -> ReactElement
bottomBar {onTimeScaleEdit,zoomSignal,timeScaleSignal} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
      where
        c :: ReactClass {classes :: {root :: String, center :: String}}
        c = component "BottomBar" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      whileMountedIx zoomSignal "BottomBar" (\this zoom -> setState this {zoom}) $
      whileMountedIx timeScaleSignal "BottomBar" (\this (TimeScale {name,units}) -> setState this {name,units})
      constructor
      where
        constructor this = do
          state <- initialState (S.readOnly zoomSignal) timeScaleSignal
          let resetZoom = IxSig.set 100.0 zoomSignal
              zoomOut = do
                z <- IxSig.get zoomSignal
                let z' | z <= 10.0 = 10.0
                       | otherwise = z - 10.0
                IxSig.set z' zoomSignal
              zoomIn = do
                z <- IxSig.get zoomSignal
                IxSig.set (z + 10.0) zoomSignal
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
                props <- getProps this
                {zoom, name, units} <- getState this
                zoomShown <- showZoom zoom
                pure $ appBar {position: absolute, className: props.classes.root}
                  [ toolbar {variant: dense}
                    [ button''
                      { color: inherit
                      , title: "Reset Zoom to 100%"
                      , onClick: mkEffectFn1 (const resetZoom)
                      , onWheel: mkEffectFn1 \e -> do
                          preventDefault e
                          stopPropagation e
                          y <- deltaY e
                          case y of
                            _ | y > 0.0 -> zoomIn
                              | y < 0.0 -> zoomOut
                              | otherwise -> pure unit
                      }
                      [ text "Zoom"
                      , nbsp
                      , text (zoomShown <> "%")
                      ]
                    , iconButton
                      { color: inherit
                      , title: "Zoom Out"
                      , disabled: zoom <= 10.0
                      , onClick: mkEffectFn1 (const zoomOut)
                      } [removeIcon]
                    , iconButton
                      { color: inherit
                      , title: "Zoom In"
                      , onClick: mkEffectFn1 (const zoomIn)
                      } [addIcon]
                    , div [RP.className props.classes.center] [] -- divider
                    , button
                      { color: inherit
                      , onClick: mkEffectFn1 (const onTimeScaleEdit)
                      , title: "TimeScale Name and Description"
                      } [text (name <> " (" <> units <> ")")]
                    ]
                  ]
            }


showZoom :: Number -> Effect String
showZoom zoom =
  case Fixed.fromNumber zoom of
    Nothing -> throw ("Can't convert number to fixed precision: " <> show zoom)
    Just (x :: Fixed.Fixed Fixed.P100) -> pure (Fixed.toString x)
