module Timeline.Data.TimelineName where

import Settings (Settings (..))

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


-- | Represents both the filename and the timeline's presented name
newtype TimelineName = TimelineName
  { title       :: String
  , description :: String
  }
derive instance genericTimelineName :: Generic TimelineName _
derive newtype instance eqTimelineName :: Eq TimelineName
derive newtype instance showTimelineName :: Show TimelineName
instance encodeJsonTimelineName :: EncodeJson TimelineName where
  encodeJson (TimelineName {title,description}) =
    "title" := title
    ~> "description" := description
    ~> jsonEmptyObject
instance decodeJsonTimelineName :: DecodeJson TimelineName where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    pure (TimelineName {title,description})



-- TODO validate filename? Meh


localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "TimelineName"


-- FIXME need to differentiate root from children; and whether a single node being a signal is even valid
-- | Chosen timeline name on boot, disregarding the shared signal
newTimelineNameSignal :: IxSignal (read :: S.READ) Settings
                      -> Effect (IxSignal (read :: S.READ, write :: S.WRITE) TimelineName)
newTimelineNameSignal settingsSignal = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure $ TimelineName
      { title: "Timeline Name"
      , description: ""
      }
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse TimelineName: " <> e
      Right x -> pure x
  sig <- make item
  let handler x = do
        Settings {localCacheTilExport} <- get settingsSignal
        when localCacheTilExport $
          setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig


clearTimelineNameCache :: Effect Unit
clearTimelineNameCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store


setDefaultTimelineName :: IxSignal (write :: S.WRITE) TimelineName
                       -> Effect Unit
setDefaultTimelineName timelineNameSignal = do
  set (TimelineName {title: "Timeline Name", description: ""}) timelineNameSignal
