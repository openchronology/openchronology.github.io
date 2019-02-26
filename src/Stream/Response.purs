module Stream.Response (Response, newResponse, parseJson) where

import Prelude ((<<<), (<$))
import Data.Either (Either (..))
import Data.Argonaut (Json)
import Web.File.Blob (Blob)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Promise (Promise, runPromise)
import Effect.Aff (Aff, makeAff, nonCanceler)


foreign import data Response :: Type

foreign import newResponseImpl :: EffectFn1 Blob Response

newResponse :: Blob -> Effect Response
newResponse = runEffectFn1 newResponseImpl

foreign import parseJsonImpl :: Response -> Promise Json

parseJson :: Response -> Aff Json
parseJson r = makeAff \resolve ->
  nonCanceler <$ runPromise (resolve <<< Right) (resolve <<< Left) (parseJsonImpl r)
