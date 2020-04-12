module Components.Dialogs.SettingsEdit (settingsEditDialog) where

import Settings (Settings (..), defaultSettings)

import Prelude
import Data.Maybe (Maybe (..))
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , getState, setState, getProps, component, createLeafElement
  )
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.FormGroup (formGroup_)
import MaterialUI.FormControlLabel (formControlLabel')
import MaterialUI.Switch (switch')
import MaterialUI.Enums (primary, secondary)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)


type State =
  { open :: Boolean
  , isEditable :: Boolean
  , localCacheTilExport :: Boolean -- is disabled when isEditable == false
  }

initialState :: IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState settingsSignal = do
  Settings {isEditable, localCacheTilExport} <- IxSig.get settingsSignal
  pure
    { open: false
    , isEditable
    , localCacheTilExport
    }

settingsEditDialog :: { settingsSignal :: IxSig.IxSignal (read :: S.READ) Settings
                      , settingsEditQueues :: IOQueues Queue Unit (Maybe Settings)
                      } -> ReactElement
settingsEditDialog
  { settingsSignal
  , settingsEditQueues: IOQueues{input,output}
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
        c' :: ReactClass {classes :: {buttons :: String}}
        c' = component "SettingsEdit" constructor'
    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handlerOpen :: _ -> Unit -> Effect Unit
          handlerOpen this _ = setState this {open: true}
          handlerChange :: _ -> Settings -> Effect Unit
          handlerChange this (Settings {isEditable,localCacheTilExport}) =
            setState this {isEditable,localCacheTilExport}
      in  whileMountedOne input handlerOpen $
          whileMountedIx settingsSignal "SettingsEdit" handlerChange constructor
      where
        constructor this = do
          state <- initialState settingsSignal
          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state
            , render: do
              let close = do
                    state' <- initialState settingsSignal
                    setState this state'
                    put output Nothing
                  restoreDefaults = case defaultSettings of
                    Settings settings -> setState this settings
                  submit = do
                    {isEditable,localCacheTilExport} <- getState this
                    put output (Just (Settings {isEditable,localCacheTilExport}))
                    setState this {open: false}
                  changeIsEditable _ isEditable =
                    setState this {isEditable}
                  changeLocalCacheTilExport _ localCacheTilExport =
                    setState this {localCacheTilExport}
              {open,isEditable,localCacheTilExport} <- getState this
              props <- getProps this
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "settingsedit-dialog-title"}
                  [ dialogTitle {id: "settingsedit-dialog-title"} [text "Settings"]
                  , dialogContent_
                    [ formGroup_
                      [ formControlLabel'
                        { control: switch'
                          { checked: isEditable
                          , onChange: mkEffectFn2 changeIsEditable
                          }
                        , label: (unsafeCoerce "Is Editable") :: ReactNode
                        }
                      , formControlLabel'
                        { control: switch'
                          { checked: localCacheTilExport
                          , onChange: mkEffectFn2 changeLocalCacheTilExport
                          , disabled: not isEditable
                          }
                        , label: (unsafeCoerce "Local Cache until Export") :: ReactNode
                        }
                      ]
                    ]
                  , dialogActions {className: props.classes.buttons}
                    [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                    , button {onClick: mkEffectFn1 (const restoreDefaults), color: secondary}
                      [text "Restore Defaults"]
                    , button
                      { onClick: mkEffectFn1 (const submit)
                      , color: primary
                      , autoFocus: true
                      } [text "Save"]
                    ]
                  ]
            }
