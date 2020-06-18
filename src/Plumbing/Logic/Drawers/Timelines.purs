module Plumbing.Logic.Drawers.Timelines where

import Components.Dialogs.NewOrEditTimeline (NewOrEditTimelineResult(..), NewOrEditTimeline)
import Components.Dialogs.DangerConfirm (DangerConfirm(..))
import Timeline.UI.Timelines (Timelines(..))
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

onClickedNewTimeline ::
  { newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe NewOrEditTimeline) (Maybe NewOrEditTimelineResult)
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  -- FIXME gonna need to scope into the timespace this belongs to, to add it and stuff
  } ->
  Effect Unit
onClickedNewTimeline { newOrEditTimelineQueues, timelinesSignal } =
  launchAff_ do
    mResult <- IOQueues.callAsync newOrEditTimelineQueues Nothing
    case mResult of
      Nothing -> pure unit
      Just result -> case result of
        NewOrEditTimeline t ->
          liftEffect do
            Timelines ts <- IxSig.get timelinesSignal
            IxSig.set (Timelines (Array.snoc ts t)) timelinesSignal
        DeleteTimeline -> liftEffect $ throw "Shouldn't be possible!"

onClickedEditTimeline ::
  { newOrEditTimelineQueues :: IOQueues Q.Queue (Maybe NewOrEditTimeline) (Maybe NewOrEditTimelineResult)
  , dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  } ->
  Int -> Effect Unit
onClickedEditTimeline { newOrEditTimelineQueues
, dangerConfirmQueues
, timelinesSignal
} index = do
  Timelines timelines <- IxSig.get timelinesSignal
  case timelines Array.!! index of
    Nothing -> throw $ "Timeline does not exist in set: " <> show index
    Just timeline ->
      launchAff_ do
        mResult <-
          IOQueues.callAsync newOrEditTimelineQueues
            $ Just
                { timeline
                , onDelete:
                    onClickedDeleteTimeline
                      { timelinesSignal
                      , dangerConfirmQueues
                      }
                      <<< Right
                }
        case mResult of
          Nothing -> pure unit
          Just result ->
            liftEffect
              $ case result of
                  NewOrEditTimeline t' -> case Array.updateAt index t' timelines of
                    Nothing -> throw $ "Timeline index does not exist in set: " <> show index
                    Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal
                  DeleteTimeline -> case Array.deleteAt index timelines of
                    Nothing -> throw $ "Timeline index does not exist in set: " <> show index
                    Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal

onClickedDeleteTimeline ::
  { dangerConfirmQueues :: IOQueues Q.Queue DangerConfirm Boolean
  , timelinesSignal :: IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines
  } ->
  Either Int (Effect Unit) -> Effect Unit
onClickedDeleteTimeline { dangerConfirmQueues, timelinesSignal } eIndexAuxAction =
  launchAff_ do
    confirmed <-
      IOQueues.callAsync dangerConfirmQueues
        $ DangerConfirm
            { content:
                [ typography { variant: body1 }
                    [ strong [] [ text "Warning!" ]
                    , nbsp
                    , text "Are you sure you want to delete this timeline?"
                    ]
                ]
            , submit: "Delete Timeline"
            }
    when confirmed $ liftEffect
      $ case eIndexAuxAction of
          Left index -> do
            Timelines timelines <- IxSig.get timelinesSignal
            case Array.deleteAt index timelines of
              Nothing -> throw $ "Timeline index does not exist in set: " <> show index
              Just timelines' -> IxSig.set (Timelines timelines') timelinesSignal
          Right auxAction -> auxAction
