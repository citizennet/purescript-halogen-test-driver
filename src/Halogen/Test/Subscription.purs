module Halogen.Test.Subscription
  ( HTML
  , Query(..)
  , component
  , subscribe
  ) where

import Pre

import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Subscription as Halogen.Subscription

data Action a
  = Event a
  | Receive (Halogen.Subscription.Emitter a)
  | Subscribe
  | Unsubscribe

type HTML m output slots =
  Halogen.HTML.HTML (Halogen.ComponentSlot (subscribe :: Halogen.Slot Query output Unit | slots) m output)

data Query (a :: Type)

-- | A component that listens to a `Halogen.Subscription.Emitter` and lets one
-- | choose what to do with emitted values in a `HalogenM` context. This is used
-- | to test form state updates in the `Form2.Managed.Halogen` component.
component ::
  forall a m output query.
  Halogen.Subscription.Emitter a ->
  (forall action slots state. a -> Halogen.HalogenM state action slots output m Unit) ->
  Halogen.Component query (Halogen.Subscription.Emitter a) output m
component emitter listen =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { finalize = Just Unsubscribe
            , handleAction =
                case _ of
                  Event a -> do
                    listen a
                  Receive emitter' -> do
                    handleUnsubscribe
                    handleSubscribe emitter'
                  Subscribe -> do
                    handleSubscribe emitter
                  Unsubscribe -> handleUnsubscribe
            , initialize = Just Subscribe
            , receive = Just <<< Receive
            }
    , initialState: const Nothing
    , render: \_ -> Halogen.HTML.text ""
    }
  where
  handleSubscribe ::
    forall slots.
    Halogen.Subscription.Emitter a ->
    Halogen.HalogenM (Maybe Halogen.SubscriptionId) (Action a) slots output m Unit
  handleSubscribe emitter' = do
    subscriptionId <- Halogen.subscribe (map Event emitter')
    Halogen.put (Just subscriptionId)

  handleUnsubscribe ::
    forall slots.
    Halogen.HalogenM (Maybe Halogen.SubscriptionId) (Action a) slots output m Unit
  handleUnsubscribe = do
    mSubscriptionId <- Halogen.get
    case mSubscriptionId of
      Nothing -> pure unit
      Just subscriptionId -> do
        Halogen.unsubscribe subscriptionId

subscribe ::
  forall m a output slots.
  Halogen.Subscription.Emitter a ->
  (forall action slots' state. a -> Halogen.HalogenM state action slots' output m Unit) ->
  HTML m output slots output
subscribe emitter listen =
  Halogen.HTML.slot
    (Proxy :: Proxy "subscribe")
    unit
    (component emitter listen)
    emitter
    identity
