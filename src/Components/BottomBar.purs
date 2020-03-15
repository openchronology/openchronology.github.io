module Components.BottomBar (bottomBar) where

import Prelude hiding (div)
import Data.Fixed (fromNumber, toString, P100, Fixed) as Fixed
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getProps, getState, setState, createLeafElement)
import React.DOM (text, div, span)
import React.DOM.Props (className, dangerouslySetInnerHTML) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (subheading, absolute, inherit, dense)
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig



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


type State = {zoom :: Number}

initialState :: IxSig.IxSignal (read :: S.READ) Number -> Effect State
initialState zoomSignal = do
  zoom <- IxSig.get zoomSignal
  pure {zoom}


bottomBar :: { onTimeScaleEdit :: Effect Unit
             , zoomSignal :: IxSig.IxSignal (read :: S.READ) Number
             } -> ReactElement
bottomBar {onTimeScaleEdit, zoomSignal} = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
    c :: ReactClass {classes :: {root :: String, center :: String}}
    c = component "BottomBar" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      whileMountedIx zoomSignal "BottomBar" (\this zoom -> setState this {zoom}) constructor
      where
        constructor = \this -> do
          state <- initialState zoomSignal
          pure
            { state
            , componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , render: do
                props <- getProps this
                {zoom} <- getState this
                zoomShown <- showZoom zoom
                pure $ appBar {position: absolute, className: props.classes.root}
                  [ toolbar {variant: dense}
                    [ typography {variant: subheading, color: inherit} [text "Zoom"]
                    , span [RP.dangerouslySetInnerHTML {__html: "&nbsp;"}] []
                    , typography {variant: subheading, color: inherit} [text zoomShown]
                    , typography {variant: subheading, color: inherit} [text "%"]
                    , div [RP.className props.classes.center] []
                    , button {color: inherit, onClick: mkEffectFn1 (const onTimeScaleEdit)} [text "TimeScale (Years)"]
                    ]
                  ]
            }


showZoom :: Number -> Effect String
showZoom zoom =
  case Fixed.fromNumber zoom of
    Nothing -> throw ("Can't convert number to fixed precision: " <> show zoom)
    Just (x :: Fixed.Fixed Fixed.P100) -> pure (Fixed.toString x)
