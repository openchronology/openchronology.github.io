module Components.Dialogs.TimeScaleEdit (timeScaleEditDialog) where

import Timeline.UI.Index.Unit (DecidedUnit(..))
import Timeline.UI.Index.MaybeLimit (DecidedMaybeLimit(..), MaybeLimit(..))
import Timeline.UI.TimeScale (TimeScale(..))
import Components.Time.Unit (unitPicker)
import Components.Time.Bounds (DecidedIntermediaryBounds(..))
import Components.Time.MaybeLimit
  ( DecidedIntermediaryMaybeLimit
  , intermediaryToMaybeLimit
  , maybeLimitPicker
  )
import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactClass
  , ReactClassConstructor
  , ReactElement
  , getState
  , setState
  , getProps
  , createLeafElement
  , component
  )
import React.DOM (text)
import React.SyntheticEvent (target)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.TextField (textField')
import MaterialUI.Typography (typography)
import MaterialUI.Enums (primary, secondary, body2)
import MaterialUI.Divider (divider')
import MaterialUI.Markdown (markdown)
import Queue.One (Queue, put)
import IOQueues (IOQueues(..))
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)

type State
  = { open :: Boolean
    , isEditable :: Boolean
    , name :: String
    , units :: String
    , description :: String
    , decidedUnit :: DecidedUnit
    , intermediaryMaybeLimit :: DecidedIntermediaryMaybeLimit
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) TimeScale ->
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState timeScaleSignal settingsSignal = do
  TimeScale { name, units, description, limit } <- IxSig.get timeScaleSignal
  Settings { isEditable } <- IxSig.get settingsSignal
  case limit of
    DecidedMaybeLimitNumber ml ->
      pure
        { open: false
        , isEditable
        , name
        , units
        , description
        , decidedUnit: DecidedUnitNumber
        , intermediaryMaybeLimit:
            case ml of
              JustLimitBounds { begin, end } ->
                { hasBegin: true
                , hasEnd: true
                , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: show begin, end: show end }
                }
              JustLimitMin { begin } ->
                { hasBegin: true
                , hasEnd: false
                , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: show begin, end: "" }
                }
              JustLimitMax { end } ->
                { hasBegin: false
                , hasEnd: true
                , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: "", end: show end }
                }
              NothingLimit ->
                { hasBegin: false
                , hasEnd: false
                , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: "", end: "" }
                }
        }

timeScaleEditDialog ::
  { timeScaleSignal :: IxSig.IxSignal ( read :: S.READ ) TimeScale
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , timeScaleEditQueues :: IOQueues Queue Unit (Maybe TimeScale)
  } ->
  ReactElement
timeScaleEditDialog { timeScaleSignal
, settingsSignal
, timeScaleEditQueues: IOQueues { input, output }
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    styles :: _
    styles theme =
      { buttons:
          { zIndex: 2
          }
      }

    c' :: ReactClass { classes :: { buttons :: String } }
    c' = component "TimeScaleEdit" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }

      -- FIXME include unit change here too?
      handlerChange :: _ -> TimeScale -> Effect Unit
      handlerChange this (TimeScale { name, units, description, limit }) = do
        setState this { name, units, description }
        setState this
          $ case limit of
              DecidedMaybeLimitNumber ml ->
                { decidedUnit: DecidedUnitNumber
                , intermediaryMaybeLimit:
                    case ml of
                      JustLimitBounds { begin, end } ->
                        { hasBegin: true
                        , hasEnd: true
                        , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: show begin, end: show end }
                        }
                      JustLimitMin { begin } ->
                        { hasBegin: true
                        , hasEnd: false
                        , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: show begin, end: "" }
                        }
                      JustLimitMax { end } ->
                        { hasBegin: false
                        , hasEnd: true
                        , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: "", end: show end }
                        }
                      NothingLimit ->
                        { hasBegin: false
                        , hasEnd: false
                        , intermediaryBounds: DecidedIntermediaryBoundsNumber { begin: "", end: "" }
                        }
                }

      handlerChangeEdit :: _ -> Settings -> Effect Unit
      handlerChangeEdit this (Settings { isEditable }) = setState this { isEditable }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx timeScaleSignal "TimeScaleEdit" handlerChange
        $ whileMountedIx settingsSignal "TimeScaleEdit" handlerChangeEdit constructor
    where
    constructor this = do
      state <- initialState timeScaleSignal settingsSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  state' <- initialState timeScaleSignal settingsSignal
                  setState this state'
                  put output Nothing

                submit = do
                  { name, units, description, intermediaryMaybeLimit } <- getState this
                  case intermediaryToMaybeLimit intermediaryMaybeLimit of
                    Nothing -> pure unit -- FIXME throw snackbar
                    Just limit -> do
                      put output (Just (TimeScale { name, units, description, limit }))
                      setState this { open: false }

                changeName e = do
                  t <- target e
                  setState this { name: (unsafeCoerce t).value }

                changeUnits e = do
                  t <- target e
                  setState this { units: (unsafeCoerce t).value }

                changeDescription e = do
                  t <- target e
                  setState this { description: (unsafeCoerce t).value }

                changeDecidedUnit u = setState this { decidedUnit: u }

                changeDecidedMaybeLimit l = setState this { intermediaryMaybeLimit: l }
              { open, isEditable, name, units, description, decidedUnit, intermediaryMaybeLimit } <- getState this
              props <- getProps this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , "aria-labelledby": "timescaleedit-dialog-title"
                    , fullWidth: true
                    }
                $ let
                    editable =
                      [ dialogTitle { id: "timescaleedit-dialog-title" } [ text "TimeScale" ]
                      , dialogContent_
                          $ [ textField'
                                { label: "Name"
                                , value: name
                                , onChange: mkEffectFn1 changeName
                                , fullWidth: true
                                }
                            , textField'
                                { label: "Units"
                                , value: units
                                , onChange: mkEffectFn1 changeUnits
                                , fullWidth: true
                                }
                            , textField'
                                { label: "Description"
                                , value: description
                                , onChange: mkEffectFn1 changeDescription
                                , multiline: true
                                , fullWidth: true
                                , rows: 4
                                }
                            , unitPicker
                                { onUnitPicked: changeDecidedUnit
                                , initialUnitPicked: Just decidedUnit
                                }
                            , maybeLimitPicker
                                { onChangeIntermediaryMaybeLimit: changeDecidedMaybeLimit
                                , intermediaryMaybeLimit
                                , decidedUnit
                                }
                            ] -- <> case editIndicies of
                      -- Nothing -> []
                      -- Just {beginIndex, endIndex} -> [beginIndex, endIndex]
                      , dialogActions { className: props.classes.buttons }
                          [ button { onClick: mkEffectFn1 (const close) } [ text "Cancel" ]
                          , button
                              { onClick: mkEffectFn1 (const submit)
                              , color: secondary
                              , autoFocus: true
                              }
                              [ text "Save" ]
                          ]
                      ]

                    notEditable =
                      [ dialogTitle { id: "timescaleedit-dialog-title" } [ text name ]
                      , dialogContent_
                          $ [ typography { gutterBottom: true, variant: body2 } [ text $ "Units: " <> units ]
                            , divider' { style: { margin: "1em 0" } }
                            , markdown description
                            ] -- <> case viewIndicies of
                      -- Nothing -> []
                      -- Just {beginIndex, endIndex} -> [beginIndex, endIndex]
                      , dialogActions { className: props.classes.buttons }
                          [ button { onClick: mkEffectFn1 (const close), color: primary } [ text "Close" ] ]
                      ]
                  in
                    if isEditable then editable else notEditable
        }
