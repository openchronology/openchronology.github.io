module Timeline.UI.Index where

import Prelude
import Data.Maybe (Maybe (..))
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
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf, elements)
import Type.Proxy (Proxy(..))

data DecidedUnit
  = DecidedUnitNumber

derive instance genericDecidedUnit :: Generic DecidedUnit _

instance eqDecidedUnit :: Eq DecidedUnit where
  eq = genericEq

instance showDecidedUnit :: Show DecidedUnit where
  show = genericShow

instance encodeJsonDecidedUnit :: EncodeJson DecidedUnit where
  encodeJson x = case x of
    DecidedUnitNumber -> encodeJson "number"

instance decodeJsonDecidedUnit :: DecodeJson DecidedUnit where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "number" -> pure DecidedUnitNumber
      _ -> fail $ "Unrecognized DecidedUnit: " <> s

instance arbitraryDecidedUnit :: Arbitrary DecidedUnit where
  arbitrary = elements $ NonEmpty DecidedUnitNumber []

data DecidedValue
  = DecidedValueNumber Number

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

data DecidedSpan
  = DecidedSpanNumber (Span Number)

makeDecidedSpan :: {start :: DecidedValue, stop :: DecidedValue} -> Maybe DecidedSpan
makeDecidedSpan {start,stop} = case Tuple start stop of
  Tuple (DecidedValueNumber start') (DecidedValueNumber stop') ->
    Just (DecidedSpanNumber {start: start', stop: stop'})
  _ -> Nothing

derive instance genericDecidedSpan :: Generic DecidedSpan _

instance eqDecidedSpan :: Eq DecidedSpan where
  eq = genericEq

instance showDecidedSpan :: Show DecidedSpan where
  show = genericShow

instance encodeJsonDecidedSpan :: EncodeJson DecidedSpan where
  encodeJson x = case x of
    DecidedSpanNumber y -> "numberSpan" := encodeJsonSpan (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedSpan :: DecodeJson DecidedSpan where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberSpan"
        DecidedSpanNumber <$> decodeJsonSpan (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedSpan :: Arbitrary DecidedSpan where
  arbitrary = oneOf $ NonEmpty (DecidedSpanNumber <$> genSpan (Proxy :: Proxy Number)) []

data DecidedBounds
  = DecidedBoundsNumber (Bounds Number)

makeDecidedBounds :: {begin :: DecidedValue, end :: DecidedValue} -> Maybe DecidedBounds
makeDecidedBounds {begin,end} = case Tuple begin end of
  Tuple (DecidedValueNumber begin') (DecidedValueNumber end') ->
    Just (DecidedBoundsNumber {begin: begin', end: end'})
  _ -> Nothing

derive instance genericDecidedBounds :: Generic DecidedBounds _

instance eqDecidedBounds :: Eq DecidedBounds where
  eq = genericEq

instance showDecidedBounds :: Show DecidedBounds where
  show = genericShow

instance encodeJsonDecidedBounds :: EncodeJson DecidedBounds where
  encodeJson x = case x of
    DecidedBoundsNumber y -> "numberBounds" := encodeJsonBounds (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedBounds :: DecodeJson DecidedBounds where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberBounds"
        DecidedBoundsNumber <$> decodeJsonBounds (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedBounds :: Arbitrary DecidedBounds where
  arbitrary = oneOf $ NonEmpty (DecidedBoundsNumber <$> genBounds (Proxy :: Proxy Number)) []

data DecidedMax
  = DecidedMaxNumber (Max Number)

derive instance genericDecidedMax :: Generic DecidedMax _

instance eqDecidedMax :: Eq DecidedMax where
  eq = genericEq

instance showDecidedMax :: Show DecidedMax where
  show = genericShow

instance encodeJsonDecidedMax :: EncodeJson DecidedMax where
  encodeJson x = case x of
    DecidedMaxNumber y -> "numberMax" := encodeJsonMax (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMax :: DecodeJson DecidedMax where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMax"
        DecidedMaxNumber <$> decodeJsonMax (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedMax :: Arbitrary DecidedMax where
  arbitrary = oneOf $ NonEmpty (DecidedMaxNumber <$> genMax (Proxy :: Proxy Number)) []

data DecidedMin
  = DecidedMinNumber (Min Number)

derive instance genericDecidedMin :: Generic DecidedMin _

instance eqDecidedMin :: Eq DecidedMin where
  eq = genericEq

instance showDecidedMin :: Show DecidedMin where
  show = genericShow

instance encodeJsonDecidedMin :: EncodeJson DecidedMin where
  encodeJson x = case x of
    DecidedMinNumber y -> "numberMin" := encodeJsonMin (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMin :: DecodeJson DecidedMin where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMin"
        DecidedMinNumber <$> decodeJsonMin (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedMin :: Arbitrary DecidedMin where
  arbitrary = oneOf $ NonEmpty (DecidedMinNumber <$> genMin (Proxy :: Proxy Number)) []

data DecidedLimit
  = DecidedLimitNumber (Limit Number)

derive instance genericDecidedLimit :: Generic DecidedLimit _

instance eqDecidedLimit :: Eq DecidedLimit where
  eq = genericEq

instance showDecidedLimit :: Show DecidedLimit where
  show = genericShow

instance encodeJsonDecidedLimit :: EncodeJson DecidedLimit where
  encodeJson x = case x of
    DecidedLimitNumber y -> "numberLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedLimit :: DecodeJson DecidedLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedLimitNumber <$> o .: "numberLimit"
    decodeNumber

instance arbitraryDecidedLimit :: Arbitrary DecidedLimit where
  arbitrary = oneOf $ NonEmpty (DecidedLimitNumber <$> arbitrary) []

data DecidedMaybeLimit
  = DecidedMaybeLimitNumber (MaybeLimit Number)

derive instance genericDecidedMaybeLimit :: Generic DecidedMaybeLimit _

instance eqDecidedMaybeLimit :: Eq DecidedMaybeLimit where
  eq = genericEq

instance showDecidedMaybeLimit :: Show DecidedMaybeLimit where
  show = genericShow

instance encodeJsonDecidedMaybeLimit :: EncodeJson DecidedMaybeLimit where
  encodeJson x = case x of
    DecidedMaybeLimitNumber y -> "numberMaybeLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedMaybeLimit :: DecodeJson DecidedMaybeLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedMaybeLimitNumber <$> o .: "numberMaybeLimit"
    decodeNumber

instance arbitraryDecidedMaybeLimit :: Arbitrary DecidedMaybeLimit where
  arbitrary = oneOf $ NonEmpty (DecidedMaybeLimitNumber <$> arbitrary) []

type Span a
  = { start :: a, stop :: a }

genSpan :: forall a. Arbitrary a => Proxy a -> Gen (Span a)
genSpan Proxy = do
  start <- arbitrary
  stop <- arbitrary
  pure { start, stop }

encodeJsonSpan :: forall a. EncodeJson a => Proxy a -> Span a -> Json
encodeJsonSpan Proxy { start, stop } = "start" := start ~> "stop" := stop ~> jsonEmptyObject

decodeJsonSpan :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Span a)
decodeJsonSpan Proxy json = do
  o <- decodeJson json
  start <- o .: "start"
  stop <- o .: "stop"
  pure { start, stop }

type Min a
  = { begin :: a }

genMin :: forall a. Arbitrary a => Proxy a -> Gen (Min a)
genMin Proxy = do
  begin <- arbitrary
  pure { begin }

encodeJsonMin :: forall a. EncodeJson a => Proxy a -> Min a -> Json
encodeJsonMin Proxy { begin } = "begin" := begin ~> jsonEmptyObject

decodeJsonMin :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Min a)
decodeJsonMin Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  pure { begin }

type Max a
  = { end :: a }

genMax :: forall a. Arbitrary a => Proxy a -> Gen (Max a)
genMax Proxy = do
  end <- arbitrary
  pure { end }

encodeJsonMax :: forall a. EncodeJson a => Proxy a -> Max a -> Json
encodeJsonMax Proxy { end } = "end" := end ~> jsonEmptyObject

decodeJsonMax :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Max a)
decodeJsonMax Proxy json = do
  o <- decodeJson json
  end <- o .: "end"
  pure { end }

type Bounds a
  = { begin :: a, end :: a }

genBounds :: forall a. Arbitrary a => Proxy a -> Gen (Bounds a)
genBounds Proxy = do
  begin <- arbitrary
  end <- arbitrary
  pure { begin, end }

encodeJsonBounds :: forall a. EncodeJson a => Proxy a -> Bounds a -> Json
encodeJsonBounds Proxy { begin, end } = "begin" := begin ~> "end" := end ~> jsonEmptyObject

decodeJsonBounds :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Bounds a)
decodeJsonBounds Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  end <- o .: "end"
  pure { begin, end }

data Limit a
  = LimitBounds (Bounds a)
  | LimitMin (Min a)
  | LimitMax (Max a)

derive instance genericLimit :: Generic a a' => Generic (Limit a) _

instance eqLimit :: Eq a => Eq (Limit a) where
  eq x y = case Tuple x y of
    Tuple (LimitBounds x') (LimitBounds y') -> x' == y'
    Tuple (LimitMin x') (LimitMin y') -> x' == y'
    Tuple (LimitMax x') (LimitMax y') -> x' == y'
    _ -> false

instance showLimit :: Show a => Show (Limit a) where
  show x = case x of
    LimitBounds y -> "(LimitBounds " <> show y <> ")"
    LimitMin y -> "(LimitMin " <> show y <> ")"
    LimitMax y -> "(LimitMax " <> show y <> ")"

instance encodeJsonLimit :: EncodeJson a => EncodeJson (Limit a) where
  encodeJson x = case x of
    LimitBounds y -> "limitBounds" := y ~> jsonEmptyObject
    LimitMin y -> "limitMin" := y ~> jsonEmptyObject
    LimitMax y -> "limitMax" := y ~> jsonEmptyObject

instance decodeJsonLimit :: DecodeJson a => DecodeJson (Limit a) where
  decodeJson json = do
    o <- decodeJson json
    let
      limitBounds = LimitBounds <$> o .: "limitBounds"

      limitMin = LimitMin <$> o .: "limitMin"

      limitMax = LimitMax <$> o .: "limitMax"
    limitBounds <|> limitMin <|> limitMax

instance arbitraryLimit :: Arbitrary a => Arbitrary (Limit a) where
  arbitrary = oneOf $ NonEmpty (LimitBounds <$> arbitrary) [ LimitMin <$> arbitrary, LimitMax <$> arbitrary ]

data MaybeLimit a
  = JustLimitBounds (Bounds a)
  | JustLimitMin (Min a)
  | JustLimitMax (Max a)
  | NothingLimit

derive instance genericMaybeLimit :: Generic a a' => Generic (MaybeLimit a) _

instance eqMaybeLimit :: Eq a => Eq (MaybeLimit a) where
  eq x y = case Tuple x y of
    Tuple NothingLimit NothingLimit -> true
    Tuple (JustLimitBounds x') (JustLimitBounds y') -> x' == y'
    Tuple (JustLimitMin x') (JustLimitMin y') -> x' == y'
    Tuple (JustLimitMax x') (JustLimitMax y') -> x' == y'
    _ -> false

instance showMaybeLimit :: Show a => Show (MaybeLimit a) where
  show x = case x of
    JustLimitBounds y -> "(JustLimitBounds " <> show y <> ")"
    JustLimitMin y -> "(JustLimitMin " <> show y <> ")"
    JustLimitMax y -> "(JustLimitMax " <> show y <> ")"
    NothingLimit -> "NothingLimit"

instance encodeJsonMaybeLimit :: EncodeJson a => EncodeJson (MaybeLimit a) where
  encodeJson x = case x of
    JustLimitBounds y -> "justLimitBounds" := y ~> jsonEmptyObject
    JustLimitMin y -> "justLimitMin" := y ~> jsonEmptyObject
    JustLimitMax y -> "justLimitMax" := y ~> jsonEmptyObject
    NothingLimit -> encodeJson "nothingLimit"

instance decodeJsonMaybeLimit :: DecodeJson a => DecodeJson (MaybeLimit a) where
  decodeJson json = do
    let
      obj = do
        o <- decodeJson json
        let
          limitBounds = JustLimitBounds <$> o .: "justLimitBounds"

          limitMin = JustLimitMin <$> o .: "justLimitMin"

          limitMax = JustLimitMax <$> o .: "justLimitMax"
        limitBounds <|> limitMin <|> limitMax

      str = do
        s <- decodeJson json
        case s of
          "nothingLimit" -> pure NothingLimit
          _ -> fail $ "Unrecognized MaybeLimit: " <> s
    obj <|> str

instance arbitraryMaybeLimit :: Arbitrary a => Arbitrary (MaybeLimit a) where
  arbitrary = oneOf $ NonEmpty (JustLimitBounds <$> arbitrary) [ JustLimitMin <$> arbitrary, JustLimitMax <$> arbitrary, pure NothingLimit ]
