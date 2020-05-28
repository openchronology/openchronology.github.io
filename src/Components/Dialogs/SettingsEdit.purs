module Components.Dialogs.SettingsEdit (settingsEditDialog) where

import Settings (Settings(..), defaultSettings)
import Prelude
import Data.Maybe (Maybe(..))
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, mkEffectFn1)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , getState
  , setState
  , getProps
  , component
  , createLeafElement
  )
import React.DOM (text, div)
import React.DOM.Props (style) as RP
import React.DOM.NonBlockingSpace (nbsp)
import React.Queue.WhileMounted (whileMountedOne)
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.Colors (red)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.FormGroup (formGroup_)
import MaterialUI.FormControlLabel (formControlLabel')
import MaterialUI.Icon (icon')
import MaterialUI.Icons.GetAppIcon (getAppIcon)
import MaterialUI.Switch (switch')
import MaterialUI.Divider (divider_)
import MaterialUI.Enums (primary, secondary, contained)
import MaterialUI.Theme (Theme)
import Queue.One (Queue, put)
import IOQueues (IOQueues(..))
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)

type State
  = { open :: Boolean
    , isEditable :: Boolean
    , isSearchable :: Boolean
    , localCacheTilExport :: Boolean -- is disabled when isEditable == false
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState settingsSignal = do
  Settings { isEditable, isSearchable, localCacheTilExport } <- IxSig.get settingsSignal
  pure
    { open: false
    , isEditable
    , isSearchable
    , localCacheTilExport
    }

styles :: Theme -> _
styles theme =
  { buttons:
      { zIndex: 2
      }
  , redButton:
      { backgroundColor: red."500"
      , "&:hover":
          { backgroundColor: red."700"
          }
      }
  }

settingsEditDialog ::
  { settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , settingsEditQueues :: IOQueues Queue Unit (Maybe Settings)
  , onNew :: Effect Unit
  , onReadEULA :: Effect Unit
  } ->
  ReactElement
settingsEditDialog { settingsSignal
, settingsEditQueues: IOQueues { input, output }
, onNew
, onReadEULA
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' ::
      ReactClass
        { classes ::
            { buttons :: String
            , redButton :: String
            }
        }
    c' = component "SettingsEdit" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }

      handlerChange :: _ -> Settings -> Effect Unit
      handlerChange this (Settings { isEditable, isSearchable, localCacheTilExport }) = setState this { isEditable, isSearchable, localCacheTilExport }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx settingsSignal "SettingsEdit" handlerChange constructor
    where
    constructor this = do
      state <- initialState settingsSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  state' <- initialState settingsSignal
                  setState this state'
                  put output Nothing

                restoreDefaults = case defaultSettings of
                  Settings settings -> setState this settings

                submit = do
                  { isEditable, isSearchable, localCacheTilExport } <- getState this
                  put output (Just (Settings { isEditable, isSearchable, localCacheTilExport }))
                  setState this { open: false }

                changeIsEditable _ isEditable = setState this { isEditable }

                changeIsSearchable _ isSearchable = setState this { isSearchable }

                changeLocalCacheTilExport _ localCacheTilExport = setState this { localCacheTilExport }
              { open, isEditable, isSearchable, localCacheTilExport } <- getState this
              props <- getProps this
              pure
                $ dialog'' { onClose: mkEffectFn1 (const close), open, "aria-labelledby": "settingsedit-dialog-title", fullWidth: true }
                    [ dialogTitle { id: "settingsedit-dialog-title" } [ text "Settings" ]
                    , dialogContent_
                        [ div [ RP.style { display: "flex", flexDirection: "row" } ]
                            [ button
                                { variant: contained
                                , color: primary
                                , href: "https://github.com/openchronology/openchronology.github.io/"
                                , target: "__blank"
                                , title: "GitHub"
                                , fullWidth: true
                                }
                                [ text "GitHub"
                                , nbsp
                                , icon' { className: "fab fa-github" }
                                ]
                            , button
                                { variant: contained
                                , color: primary
                                , href: "./openchronology-static.zip"
                                , title: "Download App"
                                , fullWidth: true
                                }
                                [ text "Download"
                                , nbsp
                                , getAppIcon
                                ]
                            ]
                        , button
                            { variant: contained
                            , onClick: mkEffectFn1 (const onReadEULA)
                            }
                            [ text "Read EULA" ]
                        , divider_ []
                        , formGroup_
                            [ formControlLabel'
                                { control:
                                    switch'
                                      { checked: isSearchable
                                      , onChange: mkEffectFn2 changeIsSearchable
                                      }
                                , label: (unsafeCoerce "Is Searchable") :: ReactNode
                                }
                            , formControlLabel'
                                { control:
                                    switch'
                                      { checked: isEditable
                                      , onChange: mkEffectFn2 changeIsEditable
                                      }
                                , label: (unsafeCoerce "Is Editable") :: ReactNode
                                }
                            , formControlLabel'
                                { control:
                                    switch'
                                      { checked: localCacheTilExport
                                      , onChange: mkEffectFn2 changeLocalCacheTilExport
                                      , disabled: not isEditable
                                      }
                                , label: (unsafeCoerce "Local Cache until Export") :: ReactNode
                                }
                            ]
                        , divider_ []
                        , button
                            { variant: contained
                            , color: primary
                            , title: "Erase and Create New Timeline"
                            , fullWidth: true
                            , className: props.classes.redButton
                            , onClick: mkEffectFn1 (const onNew)
                            }
                            [ text "Delete And Create New" ]
                        ]
                    , dialogActions { className: props.classes.buttons }
                        [ button { onClick: mkEffectFn1 (const close) } [ text "Cancel" ]
                        , button { onClick: mkEffectFn1 (const restoreDefaults), color: secondary }
                            [ text "Restore Defaults" ]
                        , button
                            { onClick: mkEffectFn1 (const submit)
                            , color: primary
                            , autoFocus: true
                            }
                            [ text "Save" ]
                        ]
                    ]
        }
