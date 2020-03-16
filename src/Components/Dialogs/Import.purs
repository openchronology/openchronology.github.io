module Components.Dialogs.Import (importDialog, ImportDialog (..)) where

import Prelude hiding (div)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import Web.File.File (File)
import Web.File.Store (getFile)
import React (ReactElement, ReactClass, ReactClassConstructor, component, setState, getState, getProps, createLeafElement)
import React.DOM (text, div)
import React.DOM.Props (className) as RP
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.CircularProgress (circularProgress')
import MaterialUI.Styles (withStyles)
import MaterialUI.Input (input')
import MaterialUI.Enums (primary)



-- | Externally supplied signals to command the dialog
data ImportDialog
  = Open
  | Close
  | Failed


type State = {open :: Boolean, loading :: Boolean}


initialState :: State
initialState = {open: false, loading: false}


importDialog :: IOQueues Queue ImportDialog (Maybe File) -- ^ Write `true` to this to open the dialog, `false` to close it
             -> ReactElement
importDialog (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        styles :: _
        styles theme =
          { loaderBackground:
            { zIndex: 1
            , width: "100%"
            , height: "100%"
            , position: "absolute"
            , backgroundColor: "rgba(255,255,255,0.5)"
            , display: "flex"
            , alignItems: "center"
            , justifyContent: "center"
            , top: 0
            , left: 0
            }
          , loader:
            { margin: theme.spacing.unit * 2
            }
          , buttons:
            { zIndex: 2
            }
          }

    c' :: ReactClass {classes :: {loaderBackground :: String, loader :: String, buttons :: String}}
    c' = component "ImportDialog" constructor'

    constructor' :: ReactClassConstructor _ State _
    constructor' =
      let handler :: _ -> ImportDialog -> Effect Unit
          handler this x = case x of
            Open -> setState this {open: true} -- don't return, leave that to the dialog
            Close -> setState this {open: false, loading: false}
            Failed -> setState this {loading: false} -- snackbar is invoked from index
      in  whileMountedOne input handler constructor
      where
        constructor this =
          let close = do
                setState this initialState
                put output Nothing
              submit = do
                mFile <- getFile "import-file"
                case mFile of
                  Nothing -> throw "No #import-file <input> node!" -- no need to throw snackbar; internal error
                  Just file -> do
                    setState this {loading: true}
                    put output (Just file)
          in  pure
                { componentDidMount: pure unit
                , componentWillUnmount: pure unit
                , state: initialState
                , render: do
                  {open,loading} <- getState this
                  props <- getProps this
                  pure $
                    dialog'' {onClose: mkEffectFn1 (const close), open, "aria-labelledby": "import-dialog-title"}
                      [ dialogTitle {id: "import-dialog-title"} [text "Import OpenChronology File"]
                      , dialogContent_ $
                        [ input' {type: "file", inputProps: {accept: ".och", id: "import-file"}}
                        ] <> if loading
                               then [ div [RP.className props.classes.loaderBackground]
                                        [circularProgress' {className: props.classes.loader}]
                                    ]
                               else []
                      , dialogActions {className: props.classes.buttons}
                        [ button {onClick: mkEffectFn1 (const close), color: primary} [text "Cancel"]
                        , button
                          { onClick: mkEffectFn1 (const submit)
                          , color: primary
                          , autoFocus: true
                          , disabled: loading
                          } [text "Import"]
                        ]
                      ]
                }
