module Components.Dialogs.SettingsEdit where

import Settings (Settings)

import Prelude
import Effect (Effect)
import Signal.Types (READ) as S
import IxSignal (IxSignal, get) as IxSig


type State =
  { open :: Boolean
  , isEditable :: Boolean
  , localCacheTilExport :: Boolean -- is disabled when isEditable == false
  }

initialState :: IxSig.IxSignal (read :: S.READ) Settings
             -> Effect State
initialState settingsSignal = do
  {isEditable, localCacheTilExport} <- IxSig.get settingsSignal
  pure
    { open: false
    , isEditable
    , localCacheTilExport
    }
