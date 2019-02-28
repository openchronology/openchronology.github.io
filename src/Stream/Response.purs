module Stream.Response (Response, newResponse, getArrayBuffer) where

import Prelude ((<<<), (<$))
import Data.Either (Either (..))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Web.File.Blob (Blob)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Promise (Promise, runPromise)
import Effect.Aff (Aff, makeAff, nonCanceler)


foreign import data Response :: Type

foreign import newResponseImpl :: EffectFn1 Blob Response

newResponse :: Blob -> Effect Response
newResponse = runEffectFn1 newResponseImpl

foreign import getArrayBufferImpl :: Response -> Promise ArrayBuffer

getArrayBuffer :: Response -> Aff ArrayBuffer
getArrayBuffer r = makeAff \resolve ->
  nonCanceler <$ runPromise (resolve <<< Right) (resolve <<< Left) (getArrayBufferImpl r)
