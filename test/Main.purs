module Test.Main where

import Timeline.UI.TimeSpaceName (TimeSpaceName)
import Timeline.UI.TimeScale (TimeScale)
import Timeline.UI.Event (Event)
import Timeline.UI.TimeSpan (TimeSpan)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan)
import Timeline.UI.Siblings (Siblings)
import Timeline.UI.Children (Children)
import Timeline.UI.Timeline (Timeline)
import Timeline.UI.Timelines (Timelines)
import Timeline.UI.Settings (Settings)


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
    jsonTest "Event" (Proxy :: Proxy Event)
    jsonTest "TimeSpan" (Proxy :: Proxy TimeSpan)
    jsonTest "EventOrTimeSpan" (Proxy :: Proxy EventOrTimeSpan)
    jsonTest "Siblings" (Proxy :: Proxy Siblings)
    jsonTest "Children" (Proxy :: Proxy Children)
    jsonTest "Timeline" (Proxy :: Proxy Timeline)
    jsonTest "Timelines" (Proxy :: Proxy Timelines)
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
