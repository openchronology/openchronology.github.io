module Test.Main where

import Timeline.Data.TimeSpaceName (TimeSpaceName)
import Timeline.Data.TimeScale (TimeScale)
import Settings (Settings)


import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Identity (Identity)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.QuickCheck (class Arbitrary, quickCheck)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main =
  launchAff_ (runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] tests)


tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Json" do
    jsonTest "Settings" (Proxy :: Proxy Settings)
    jsonTest "TimeSpaceName" (Proxy :: Proxy TimeSpaceName)
    jsonTest "TimeScale" (Proxy :: Proxy TimeScale)
  where
    jsonTest :: forall a
              . Arbitrary a
             => Eq a
             => EncodeJson a
             => DecodeJson a
             => String -> Proxy a -> _
    jsonTest name = it name <<< liftEffect <<< quickCheck <<< jsonIso


jsonIso :: forall a
         . Eq a
        => EncodeJson a
        => DecodeJson a
        => Proxy a -> a -> Boolean
jsonIso Proxy x = decodeJson (encodeJson x) == Right x
