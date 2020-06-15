module Components.Dialogs.TimeSpaceNameEdit (timeSpaceNameEditDialog) where

import Timeline.UI.TimeSpaceName (TimeSpaceName(..))
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
import MaterialUI.Theme (Theme)
import MaterialUI.Markdown (markdown)
import MaterialUI.Enums (secondary)
import Queue.One (Queue, put)
import IOQueues (IOQueues(..))
import Zeta.Types (READ) as S
import IxZeta (IxSignal, get) as IxSig
import Unsafe.Coerce (unsafeCoerce)

type State
  = { open :: Boolean
    , isEditable :: Boolean
    , title :: String
    , description :: String
    , wysiwyg :: Boolean
    }

initialState ::
  IxSig.IxSignal ( read :: S.READ ) TimeSpaceName ->
  IxSig.IxSignal ( read :: S.READ ) Settings ->
  Effect State
initialState timeSpaceNameSignal settingsSignal = do
  TimeSpaceName { title, description } <- IxSig.get timeSpaceNameSignal
  Settings { isEditable } <- IxSig.get settingsSignal
  pure
    { open: false
    , isEditable
    , title
    , description
    , wysiwyg: true
    }

styles :: Theme -> _
styles theme =
  { buttons:
      { zIndex: 2
      }
  }

timeSpaceNameEditDialog ::
  { timeSpaceNameSignal :: IxSig.IxSignal ( read :: S.READ ) TimeSpaceName
  , settingsSignal :: IxSig.IxSignal ( read :: S.READ ) Settings
  , timeSpaceNameEditQueues :: IOQueues Queue Unit (Maybe TimeSpaceName)
  } ->
  ReactElement
timeSpaceNameEditDialog { timeSpaceNameSignal
, settingsSignal
, timeSpaceNameEditQueues: IOQueues { input, output }
} = createLeafElement c {}
  where
  c :: ReactClass {}
  c = withStyles styles c'
    where
    c' :: ReactClass { classes :: { buttons :: String } }
    c' = component "TimeSpaceNameEdit" constructor'

  constructor' :: ReactClassConstructor _ State _
  constructor' =
    let
      handlerOpen :: _ -> Unit -> Effect Unit
      handlerOpen this _ = setState this { open: true }

      handlerChange :: _ -> TimeSpaceName -> Effect Unit
      handlerChange this (TimeSpaceName { title, description }) = setState this { title, description }

      handlerChangeEdit :: _ -> Settings -> Effect Unit
      handlerChangeEdit this (Settings { isEditable }) = setState this { isEditable }
    in
      whileMountedOne input handlerOpen
        $ whileMountedIx timeSpaceNameSignal "TimeSpaceNameEdit" handlerChange
        $ whileMountedIx settingsSignal "TimeSpaceNameEdit" handlerChangeEdit constructor
    where
    constructor this = do
      state <- initialState timeSpaceNameSignal settingsSignal
      pure
        { componentDidMount: pure unit
        , componentWillUnmount: pure unit
        , state
        , render:
            do
              let
                close = do
                  state' <- initialState timeSpaceNameSignal settingsSignal
                  setState this state'
                  put output Nothing

                submit = do
                  { title, description } <- getState this
                  put output (Just (TimeSpaceName { title, description }))
                  setState this { open: false }

                changeTitle e = do
                  t <- target e
                  setState this { title: (unsafeCoerce t).value }

                changeDescription e = do
                  t <- target e
                  setState this { description: (unsafeCoerce t).value }

                changeDescription' x = do
                  s <- x
                  setState this { description: s }

                changeEditor _ val = setState this { wysiwyg: if unsafeCoerce val == 0 then true else false }
              { open, isEditable, title, description, wysiwyg } <- getState this
              props <- getProps this
              pure
                $ dialog''
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , "aria-labelledby": "timeSpaceNameedit-dialog-title"
                    , fullWidth: true
                    }
                $ let
                    editable =
                      [ dialogTitle { id: "timeSpaceNameedit-dialog-title" } [ text "Edit TimeSpace" ] -- FIXME New timespace?
                      , dialogContent_
                          [ textField'
                              { label: "Title"
                              , value: title
                              , onChange: mkEffectFn1 changeTitle
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
                          ]
                      -- , tabs
                      --   { value: (unsafeCoerce (if wysiwyg then 0 else 1) :: Any)
                      --   , indicatorColor: primary
                      --   , textColor: primary
                      --   , onChange: mkEffectFn2 changeEditor
                      --   }
                      --   [ tab' {label: "Editor"}
                      --   , tab' {label: "Plain"}
                      --   ]
                      -- , if wysiwyg
                      --     then editor {defaultValue: description, onChange: mkEffectFn1 changeDescription' }
                      --     else textField'
                      --             { label: "Description"
                      --             , value: description
                      --             , onChange: mkEffectFn1 changeDescription
                      --             , multiline: true
                      --             , fullWidth: true
                      --             , rows: 4
                      --             }
                      -- ]
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
                      [ dialogTitle { id: "timeSpaceNameedit-dialog-title" } [ text title ]
                      , dialogContent_
                          [ markdown description ]
                      , dialogActions { className: props.classes.buttons }
                          [ button { onClick: mkEffectFn1 (const close) } [ text "Close" ] ]
                      ]
                  in
                    if isEditable then editable else notEditable
        }
