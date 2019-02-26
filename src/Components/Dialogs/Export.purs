module Components.Dialogs.Export (exportDialog) where

import Prelude
import Data.Maybe (Maybe (..))
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, WRITE)
import Queue.Types (allowReading)
import Web.File.Url (revokeObjectURL)
import Web.File.Blob (Blob)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import React (ReactElement, ReactClass, ReactClassConstructor, component, setState, getState, createLeafElement)
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Input (input')
import MaterialUI.Enums (primary)
import Unsafe.Coerce (unsafeCoerce)
import Global.Unsafe (unsafeEncodeURIComponent)



type State = {open :: Boolean, value :: String}

initialState :: State
initialState = {open: false, value: ""}


exportDialog :: Queue (write :: WRITE) String -- ^ Write to this to open the dialog
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
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "import-dialog-title"}
                  [ dialogTitle {id: "import-dialog-title"} [text "Export OpenChronology File"]
                  , dialogActions_
                    [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                    , let params :: {href :: String}
                          params = unsafeCoerce
                            { href: "data:text/json;charset=utf-8," <> unsafeEncodeURIComponent value
                            , color: primary
                            , autoFocus: true
                            , download: "foodoc.och"
                            }
                      in  button params [text "Export"]
                    ]
                  ]
            }
