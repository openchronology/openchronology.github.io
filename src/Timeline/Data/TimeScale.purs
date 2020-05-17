module Timeline.Data.TimeScale where

import Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , (:=)
  , (.:)
  , (~>)
  , jsonEmptyObject
  , stringify
  , jsonParser
  )
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Effect (Effect)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, get, set, subscribeDiffLight)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.UTF8String (genString)

newtype TimeScale
  = TimeScale
  { name :: String
  , units :: String
  , description :: String
  -- , morphism :: Equation -- change this for different mappings - for now, we're linear
  -- , beginIndex  :: a
  -- , endIndex    :: a
  }

derive instance genericTimeScale :: Generic TimeScale _

derive newtype instance eqTimeScale :: Eq TimeScale

derive newtype instance showTimeScale :: Show TimeScale

instance arbitraryTimeScale :: Arbitrary TimeScale where
  arbitrary = do
    name <- genString
    units <- genString
    description <- genString
    pure (TimeScale { name, units, description })

instance encodeJsonTimeScale :: EncodeJson TimeScale where
  encodeJson (TimeScale { name, units, description }) =
    "name" := name
      ~> "units"
      := units
      ~> "description"
      := description
      ~> jsonEmptyObject

instance decodeJsonTimeScale :: DecodeJson TimeScale where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    units <- o .: "units"
    description <- o .: "description"
    pure (TimeScale { name, units, description })

localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "TimeScale"

newTimeScaleSignal ::
  IxSignal ( read :: S.READ ) Settings ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) TimeScale)
newTimeScaleSignal settingsSignal = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing ->
      pure
        $ TimeScale
            { name: "TimeScale Name"
            , units: "Years"
            , description: ""
            }
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse TimeScale: " <> e
      Right x -> pure x
  sig <- make item
  let
    handler x = do
      Settings { localCacheTilExport } <- get settingsSignal
      when localCacheTilExport
        $ setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

clearTimeScaleCache :: Effect Unit
clearTimeScaleCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store

setDefaultTimeScale ::
  IxSignal ( write :: S.WRITE ) TimeScale ->
  Effect Unit
setDefaultTimeScale timeScaleSignal = set (TimeScale { name: "TimeScale Name", units: "Years", description: "" }) timeScaleSignal
