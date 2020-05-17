module Components.Snackbar (snackbars, SnackbarVariant(..), SnackbarContent) where

{-|

The Snackbar is just what relays little notifications to the user whenever something
needs to be said; like an interactive footnote. To use this, just bind an instance
of the react component to the top-level of the app, and write to the Queue to send
a message.

-}
import Prelude
import Data.Time.Duration (Milliseconds)
import Data.Nullable (toNullable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.TSCompat (OneOf)
import Data.TSCompat.React (ReactNode)
import Data.Array (findIndex, deleteAt, snoc)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import React
  ( ReactElement
  , ReactClass
  , ReactClassConstructor
  , createLeafElement
  , component
  , getProps
  , getState
  , setState
  , toElement
  )
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (inherit)
import MaterialUI.Colors (green, amber)
import MaterialUI.Snackbar (snackbar'') as S
import MaterialUI.SnackbarContent (snackbarContent')
import MaterialUI.Button (button)
import Queue.Types (READ)
import Queue.One (Queue)
import Record (get) as Rec
import Data.Symbol (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | The type of snackbar being sent
data SnackbarVariant
  = Success
  | Error
  | Info
  | Warning

-- | All the information necessary to send a snackbar. Note that if the timeout is Nothing,
-- | the user will have to close the message to get rid of it.
type SnackbarContent
  = { variant :: SnackbarVariant
    , message :: String
    , timeout :: Maybe Milliseconds
    }

-- | Shows all the active snackbars, and pulls them from the Queue.
snackbars ::
  -- | Write to this to add a snackbar to the stack
  Queue ( read :: READ ) SnackbarContent ->
  ReactElement
snackbars snackQueue = createLeafElement c {}
  where
  c :: ReactClass {}
  c = component "OCHSnackbars" constructor
    where
    constructor :: ReactClassConstructor _ { snacks :: Array (Tuple Int SnackbarContent), counter :: Int } _
    constructor =
      let
        go this value = do
          { counter, snacks } <- getState this
          setState this
            { snacks: snoc snacks (Tuple counter value)
            , counter: counter + 1
            }
      in
        whileMountedOne snackQueue go constructor'
      where
      constructor' this =
        let
          deleteIx :: Int -> Effect Unit
          deleteIx i = do
            { snacks } <- getState this
            case findIndex (\(Tuple i' _) -> i == i') snacks of
              Nothing -> pure unit
              Just ix -> case deleteAt ix snacks of
                Nothing -> pure unit
                Just snacks' -> setState this { snacks: snacks' }
        in
          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: { snacks: [], counter: 0 }
            , render:
                do
                  { snacks } <- getState this
                  pure $ toElement $ map (\(Tuple i s) -> snackbar i (deleteIx i) s) snacks
            }

-- | Individual snackbars with cosmetic details.
snackbar ::
  -- | Key
  Int ->
  -- | Call this when exited
  Effect Unit ->
  SnackbarContent ->
  ReactElement
snackbar key onExited { variant, message, timeout } = createLeafElement c' {}
  where
  c' :: ReactClass {}
  c' = withStyles styles c
    where
    styles :: _
    styles theme =
      { success:
          { backgroundColor: Rec.get (SProxy :: SProxy "600") green
          }
      , error:
          { backgroundColor: theme.palette.error.dark
          }
      , info:
          { backgroundColor: theme.palette.primary.dark
          }
      , warning:
          { backgroundColor: Rec.get (SProxy :: SProxy "700") amber
          }
      }

  c :: ReactClass { classes :: { success :: String, error :: String, info :: String, warning :: String } }
  c = component "OCHSnackbar" constructor
    where
    constructor :: ReactClassConstructor _ { open :: Boolean } _
    constructor this =
      let
        onClose = setState this { open: false }
      in
        pure
          { state: { open: true }
          , render:
              do
                { open } <- getState this
                props <- getProps this
                pure
                  $ S.snackbar''
                      { open
                      , key: (unsafeCoerce key) :: OneOf ( typed :: String, typed :: Number )
                      , onClose: mkEffectFn2 \_ _ -> onClose
                      , autoHideDuration: unsafeCoerce (toNullable timeout) :: Number
                      , onExited: mkEffectFn1 (const onExited)
                      }
                      [ snackbarContent'
                          { className:
                              case variant of
                                Success -> props.classes.success
                                Error -> props.classes.error
                                Info -> props.classes.info
                                Warning -> props.classes.warning
                          , message: (unsafeCoerce message) :: ReactNode
                          , action:
                              let
                                x :: ReactNode
                                x =
                                  unsafeCoerce
                                    $ button
                                        { onClick: mkEffectFn1 (const onClose)
                                        , color: inherit
                                        }
                                        [ text "Close" ]
                              in
                                x
                          }
                      ]
          }
