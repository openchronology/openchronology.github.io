module Settings where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson
  , encodeJson, decodeJson, (:=), (.:), (~>), jsonEmptyObject
  , stringify, jsonParser)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Effect (Effect)
import Effect.Exception (throw)
import Signal.Types (READ, WRITE) as S
import IxSignal (IxSignal, make, get, set, subscribeDiffLight)


newtype Settings = Settings
  { isEditable :: Boolean -- ^ `true` when opening a new timeline, `false` when loading one
  , localCacheTilExport :: Boolean -- ^ `true` by default - store local changes until export
  }
derive instance genericSettings :: Generic Settings _
derive newtype instance eqSettings :: Eq Settings
derive newtype instance showSettings :: Show Settings
instance encodeJsonSettings :: EncodeJson Settings where
  encodeJson (Settings {isEditable,localCacheTilExport}) =
    "isEditable" := isEditable
    ~> "localCacheTilExport" := localCacheTilExport
    ~> jsonEmptyObject
instance decodeJsonSettings :: DecodeJson Settings where
  decodeJson json = do
    o <- decodeJson json
    isEditable <- o .: "isEditable"
    localCacheTilExport <- o .: "localCacheTilExport"
    pure (Settings {isEditable,localCacheTilExport})


localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "Settings"


newSettingsSignal :: { wasOpenedByShareLink :: Boolean
                     } -> Effect (IxSignal (read :: S.READ, write :: S.WRITE) Settings)
newSettingsSignal {wasOpenedByShareLink} = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure $ Settings
      { isEditable: not wasOpenedByShareLink
      , localCacheTilExport: true
      }
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse Settings: " <> e
      Right x -> pure x
  sig <- make item
  -- Always store settings
  let handler x = setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig


defaultSettings :: Settings
defaultSettings =
  Settings {isEditable: true, localCacheTilExport: true}
