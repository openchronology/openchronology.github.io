module Components.Dialogs.Import (importDialog) where

import Prelude
import Data.Maybe (Maybe (..))
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import Web.File.File (File)
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



type State = {open :: Boolean}


initialState :: State
initialState = {open: false}


importDialog :: IOQueues Queue Unit (Maybe File) -- ^ Write to this to open the dialog
             -> ReactElement
importDialog (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "ImportDialog" constructor'

    constructor' :: ReactClassConstructor {} State _
    constructor' = whileMountedOne input (\this _ -> setState this {open: true}) constructor

    constructor :: ReactClassConstructor {} State _
    constructor this =
      let close = do
            setState this initialState
            put output Nothing
          submit = do
            doc <- (toNonElementParentNode <<< toDocument) <$> (window >>= document)
            mEl <- getElementById "import-file" doc
            case mEl of
              Nothing -> throw "No #import-file <input> node!"
              Just el -> put output (item 0 (unsafeCoerce el).files)
      in  pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initialState
            , render: do
              {open} <- getState this
              pure $
                dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "import-dialog-title"}
                  [ dialogTitle {id: "import-dialog-title"} [text "Import OpenChronology File"]
                  , dialogContent_
                    [ input' {type: "file", inputProps: {accept: ".och", id: "import-file"}}
                    ]
                  , dialogActions_
                    [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                    , button {onClick: mkEffectFn1 (const submit), color: primary, autoFocus: true} [text "Import"]
                    ]
                  ]
            }
