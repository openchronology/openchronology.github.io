module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import Components.Dialogs.Export (exportDialog)
import WithRoot (withRoot)
import Stream.Response (newResponse, parseJson)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (Json, encodeJson, stringify)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Aff (runAff_)
import Effect.Ref (Ref)
import Queue.One (new, put) as Q
import Queue.Types (writeOnly) as Q
import IOQueues (new, callAsync) as IOQueues
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)



index :: { stateRef :: Ref Unit
         }
      -> ReactElement
index {stateRef} = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          importQueues <- IOQueues.new
          exportQueue <- Q.writeOnly <$> Q.new

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              onImport = runAff_ resolve $ do
                mF <- IOQueues.callAsync importQueues unit
                case mF of
                  Nothing -> pure unit
                  Just f -> do
                    r <- liftEffect (newResponse f)
                    json <- parseJson r
                    liftEffect $ do
                      log $ unsafeCoerce json

              onExport = do
                let x :: Json
                    x = encodeJson "yo dawg"
                Q.put exportQueue (stringify x)
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar {onImport, onExport}
              , typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , importDialog importQueues
              , exportDialog exportQueue
              ]
            }
