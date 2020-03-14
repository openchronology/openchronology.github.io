module Components.Dialogs.Export (exportDialog) where

import Prelude
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.MediaType (MediaType (..))
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Queue.One (Queue, WRITE)
import Queue.Types (allowReading)
import React (ReactElement, ReactClass, ReactClassConstructor, component, setState, getState, createLeafElement)
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Enums (primary)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Store (makeBase64Href)


type State = {open :: Boolean, value :: ArrayBuffer}


initialState :: State
initialState = {open: false, value: unsafePerformEffect (AB.empty 0)}


exportDialog :: Queue (write :: WRITE) ArrayBuffer -- ^ Write to this to open the dialog
             -> ReactElement
exportDialog input = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "ExportDialog" constructor'

    constructor' :: ReactClassConstructor {} State _
    constructor' = whileMountedOne (allowReading input) (\this value -> setState this {open: true, value}) constructor

    constructor :: ReactClassConstructor {} State _
    constructor this =
      let close = setState this initialState
      in  pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initialState
            , render: do
              {open, value} <- getState this
              href <- makeBase64Href (MediaType "application/openchronology") value
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "import-dialog-title"}
                  [ dialogTitle {id: "import-dialog-title"} [text "Export OpenChronology File"]
                  , dialogActions_
                    [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                    , let params :: {href :: String}
                          params = unsafeCoerce
                            { href
                            , color: primary
                            , autoFocus: true
                            , download: "foodoc.och"
                            }
                      in  button params [text "Export"]
                    ]
                  ]
            }
