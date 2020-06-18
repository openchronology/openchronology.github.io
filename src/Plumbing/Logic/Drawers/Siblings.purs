module Plumbing.Logic.Drawers.Siblings where

import Components.Dialogs.NewOrEditEventOrTimeSpan (NewOrEditEventOrTimeSpanResult(..), NewOrEditEventOrTimeSpan)
import Components.Dialogs.DangerConfirm (DangerConfirm(..))
import Timeline.UI.Siblings (Siblings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (snoc, (!!), updateAt, deleteAt) as Array
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.DOM (text, strong)
import React.DOM.NonBlockingSpace (nbsp)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (body1)
import Queue.One (Queue) as Q
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import Zeta.Types (WRITE, READ) as S
import IxZeta (IxSignal, get, set) as IxSig

onClickedNewEventOrTimeSpanSiblings ::
  { newOrEditEventOrTimeSpanQueues :: IOQueues Q.Queue (Maybe NewOrEditEventOrTimeSpan) (Maybe NewOrEditEventOrTimeSpanResult)
  , siblingsSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Siblings
  -- FIXME gonna need to scope into the timespace this belongs to, to add it and stuff
  } ->
  Effect Unit
onClickedNewEventOrTimeSpanSiblings { newOrEditEventOrTimeSpanQueues, siblingsSignal } =
  launchAff_ do
    mResult <- IOQueues.callAsync newOrEditEventOrTimeSpanQueues Nothing
    case mResult of
      Nothing -> pure unit
      Just result -> case result of
        NewOrEditEventOrTimeSpan t ->
          liftEffect do
            Siblings ts <- IxSig.get siblingsSignal
            IxSig.set (Siblings (Array.snoc ts t)) siblingsSignal
        DeleteEventOrTimeSpan -> liftEffect $ throw "Shouldn't be possible!"

onClickedEditEventOrTimeSpanSiblings ::
  { newOrEditEventOrTimeSpanQueues :: IOQueues Q.Queue (Maybe NewOrEditEventOrTimeSpan) (Maybe NewOrEditEventOrTimeSpanResult)
  , siblingsSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Siblings
  , dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  } ->
  Int -> Effect Unit
onClickedEditEventOrTimeSpanSiblings { newOrEditEventOrTimeSpanQueues
, siblingsSignal
, dangerConfirmQueues
} index = do
  Siblings siblings <- IxSig.get siblingsSignal
  case siblings Array.!! index of
    Nothing -> throw $ "EventOrTimeSpan does not exist in set: " <> show index
    Just eventOrTimeSpan ->
      launchAff_ do
        mResult <-
          IOQueues.callAsync newOrEditEventOrTimeSpanQueues
            $ Just
                { eventOrTimeSpan
                , onDelete:
                    onClickedDeleteEventOrTimeSpanSiblings
                      { siblingsSignal
                      , dangerConfirmQueues
                      }
                      <<< Right
                }
        case mResult of
          Nothing -> pure unit
          Just result ->
            liftEffect
              $ case result of
                  NewOrEditEventOrTimeSpan t' -> case Array.updateAt index t' siblings of
                    Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
                    Just siblings' -> IxSig.set (Siblings siblings') siblingsSignal
                  DeleteEventOrTimeSpan -> case Array.deleteAt index siblings of
                    Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
                    Just siblings' -> IxSig.set (Siblings siblings') siblingsSignal

onClickedDeleteEventOrTimeSpanSiblings ::
  { dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , siblingsSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Siblings
  } ->
  Either Int (Effect Unit) -> Effect Unit
onClickedDeleteEventOrTimeSpanSiblings { dangerConfirmQueues, siblingsSignal } eIndexAuxAction =
  launchAff_ do
    confirmed <-
      IOQueues.callAsync dangerConfirmQueues
        $ DangerConfirm
            { content:
                [ typography { variant: body1 }
                    [ strong [] [ text "Warning!" ]
                    , nbsp
                    , text "Are you sure you want to delete this eventOrTimeSpan?" -- FIXME fix this message
                    ]
                ]
            , submit: "Delete EventOrTimeSpan" -- FIXME fix this message
            }
    when confirmed $ liftEffect
      $ case eIndexAuxAction of
          Left index -> do
            Siblings siblings <- IxSig.get siblingsSignal
            case Array.deleteAt index siblings of
              Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
              Just siblings' -> IxSig.set (Siblings siblings') siblingsSignal
          Right auxAction -> auxAction
