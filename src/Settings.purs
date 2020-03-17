module Settings where

import Prelude
import Effect (Effect)
import Signal.Types (READ, WRITE) as S
import IxSignal (IxSignal, make)


type Settings =
  { isEditable :: Boolean -- ^ `true` when opening a new timeline, `false` when loading one
  , localCacheTilExport :: Boolean -- ^ `true` by default - store local changes until export
  }


newSettingsSignal :: { wasOpenedByFile :: Boolean
                     } -> Effect (IxSignal (read :: S.READ, write :: S.WRITE) Settings)
newSettingsSignal {wasOpenedByFile} = do
  make
    { isEditable: not wasOpenedByFile
    , localCacheTilExport: true
    }
