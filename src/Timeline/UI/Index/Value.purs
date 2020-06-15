module Timeline.UI.Index.Value where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , Json
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  , fail
  )
import Control.Alternative ((<|>))
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf, elements)
import Type.Proxy (Proxy(..))

data DecidedValue
  = DecidedValueNumber Number

-- | Creates a string out of a `DecidedValue`, to be used in a text field.
intermediaryDecidedValue :: DecidedValue -> String
intermediaryDecidedValue v = case v of
  DecidedValueNumber n -> show n

-- FIXME date picker?
derive instance genericDecidedValue :: Generic DecidedValue _

instance eqDecidedValue :: Eq DecidedValue where
  eq = genericEq

instance showDecidedValue :: Show DecidedValue where
  show = genericShow

instance encodeJsonDecidedValue :: EncodeJson DecidedValue where
  encodeJson x = case x of
    DecidedValueNumber y -> "numberValue" := y ~> jsonEmptyObject

instance decodeJsonDecidedValue :: DecodeJson DecidedValue where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedValueNumber <$> o .: "numberValue"
    decodeNumber

instance arbitraryDecidedValue :: Arbitrary DecidedValue where
  arbitrary = oneOf $ NonEmpty (DecidedValueNumber <$> arbitrary) []
