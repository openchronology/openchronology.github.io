module Plumbing.Logic.Drawers.Children where

import Components.Dialogs.NewOrEditEventOrTimeSpan (NewOrEditEventOrTimeSpanResult(..), NewOrEditEventOrTimeSpan)
import Components.Dialogs.DangerConfirm (DangerConfirm(..))
import Timeline.UI.Children (Children(..))
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

onClickedNewEventOrTimeSpanChildren ::
  { newOrEditEventOrTimeSpanQueues :: IOQueues Q.Queue (Maybe NewOrEditEventOrTimeSpan) (Maybe NewOrEditEventOrTimeSpanResult)
  , childrenSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Children
  -- FIXME gonna need to scope into the timespace this belongs to, to add it and stuff
  } ->
  Effect Unit
onClickedNewEventOrTimeSpanChildren { newOrEditEventOrTimeSpanQueues, childrenSignal } =
  launchAff_ do
    mResult <- IOQueues.callAsync newOrEditEventOrTimeSpanQueues Nothing
    case mResult of
      Nothing -> pure unit
      Just result -> case result of
        NewOrEditEventOrTimeSpan t ->
          liftEffect do
            Children ts <- IxSig.get childrenSignal
            IxSig.set (Children (Array.snoc ts t)) childrenSignal
        DeleteEventOrTimeSpan -> liftEffect $ throw "Shouldn't be possible!"

onClickedEditEventOrTimeSpanChildren ::
  { newOrEditEventOrTimeSpanQueues :: IOQueues Q.Queue (Maybe NewOrEditEventOrTimeSpan) (Maybe NewOrEditEventOrTimeSpanResult)
  , childrenSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Children
  , dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  } ->
  Int -> Effect Unit
onClickedEditEventOrTimeSpanChildren { newOrEditEventOrTimeSpanQueues
, childrenSignal
, dangerConfirmQueues
} index = do
  Children children <- IxSig.get childrenSignal
  case children Array.!! index of
    Nothing -> throw $ "EventOrTimeSpan does not exist in set: " <> show index
    Just eventOrTimeSpan ->
      launchAff_ do
        mResult <-
          IOQueues.callAsync newOrEditEventOrTimeSpanQueues
            $ Just
                { eventOrTimeSpan
                , onDelete:
                    onClickedDeleteEventOrTimeSpanChildren
                      { dangerConfirmQueues
                      , childrenSignal
                      }
                      <<< Right
                }
        case mResult of
          Nothing -> pure unit
          Just result ->
            liftEffect
              $ case result of
                  NewOrEditEventOrTimeSpan t' -> case Array.updateAt index t' children of
                    Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
                    Just children' -> IxSig.set (Children children') childrenSignal
                  DeleteEventOrTimeSpan -> case Array.deleteAt index children of
                    Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
                    Just children' -> IxSig.set (Children children') childrenSignal

onClickedDeleteEventOrTimeSpanChildren ::
  { dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , childrenSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Children
  } ->
  Either Int (Effect Unit) -> Effect Unit
onClickedDeleteEventOrTimeSpanChildren { dangerConfirmQueues, childrenSignal } eIndexAuxAction =
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
            , submit: "Delete EventOrTimeSpan"
            }
    when confirmed $ liftEffect
      $ case eIndexAuxAction of
          Left index -> do
            Children children <- IxSig.get childrenSignal
            case Array.deleteAt index children of
              Nothing -> throw $ "EventOrTimeSpan index does not exist in set: " <> show index
              Just children' -> IxSig.set (Children children') childrenSignal
          Right auxAction -> auxAction
