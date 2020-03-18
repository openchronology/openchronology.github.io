module Components.Dialogs.Export (exportDialog, ExportDialog (..)) where

import Prelude
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.MediaType (MediaType (..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Queue.One (Queue, READ)
import React (ReactElement, ReactClass, ReactClassConstructor, component, setState, getState, createLeafElement)
import React.DOM (text, strong, span)
import React.DOM.Props (dangerouslySetInnerHTML) as RP
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Enums (primary, subheading)
import MaterialUI.Typography (typography)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Store (arrayBufferToBlob)
import Web.File.Url (createObjectURL)


newtype ExportDialog = ExportDialog
  { buffer   :: ArrayBuffer
  , filename :: String
  }


type State = {open :: Boolean, buffer :: ArrayBuffer, filename :: String}


initialState :: State
initialState = {open: false, buffer: unsafePerformEffect (AB.empty 0), filename: ""}


exportDialog :: { exportQueue     :: Queue (read :: READ) ExportDialog -- ^ Write to this to open the dialog
                , onClickedExport :: Effect Unit
                } -> ReactElement
exportDialog {exportQueue, onClickedExport} = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "ExportDialog" constructor'

    constructor' :: ReactClassConstructor {} State _
    constructor' = whileMountedOne
      exportQueue
      (\this (ExportDialog {buffer,filename}) -> setState this {open: true, buffer, filename})
      constructor

    constructor :: ReactClassConstructor {} State _
    constructor this = pure
      { componentDidMount: pure unit
      , componentWillUnmount: pure unit
      , state: initialState
      , render: do
        let close = setState this initialState
        {open, buffer, filename} <- getState this
        href <- createObjectURL (arrayBufferToBlob (MediaType "application/openchronology") buffer)
        pure $
          dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "import-dialog-title"}
            [ dialogTitle {id: "import-dialog-title"} [text "Export OpenChronology File"]
            , dialogContent_
              [ typography {gutterBottom: true, variant: subheading}
                [ strong [] [text "Warning!"]
                , span [RP.dangerouslySetInnerHTML {__html: "&nbsp;"}] []
                , text "Clicking \"Export\" will delete any unsaved data!"
                ]
              ]
            , dialogActions_
              [ button
                  { onClick: mkEffectFn1 (const close)
                  , color: primary
                  } [text "Cancel"]
              , let params :: {href :: String}
                    params = unsafeCoerce
                      { href
                      , color: primary
                      , autoFocus: true
                      , download: filename <> ".och"
                      , onClick: mkEffectFn1 (const onClickedExport)
                      }
                in  button params [text "Export"]
              ]
            ]
      }
