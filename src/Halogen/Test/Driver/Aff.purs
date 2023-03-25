-- | Adapted from `Halogen.Aff.Driver`
module Halogen.Test.Driver.Aff
  ( RenderSpec
  , runUI
  ) where

import CitizenNet.Prelude

import Control.Monad.Fork.Class as Control.Monad.Fork.Class
import Control.Monad.Rec.Class as Control.Monad.Rec.Class
import Control.Parallel as Control.Parallel
import Data.List as Data.List
import Data.Map as Data.Map
import Data.Maybe as Data.Maybe
import Data.Traversable as Data.Traversable
import Effect.Aff as Effect.Aff
import Effect.Exception as Effect.Exception
import Effect.Ref as Effect.Ref
import Halogen as Halogen
import Halogen.Aff.Driver.Eval as Halogen.Aff.Driver.Eval
import Halogen.Aff.Driver.State as Halogen.Aff.Driver.State
import Halogen.Component as Halogen.Component
import Halogen.Data.Slot as Halogen.Data.Slot
import Halogen.HTML as Halogen.HTML
import Halogen.Query.HalogenQ as Halogen.Query.HalogenQ
import Halogen.Query.Input as Halogen.Query.Input
import Halogen.Subscription as Halogen.Subscription

type RenderSpec r =
  { render ::
      forall s act ps o.
      (Halogen.Query.Input.Input act -> Effect Unit) ->
      (Halogen.Component.ComponentSlotBox ps Aff act -> Effect (Halogen.Aff.Driver.State.RenderStateX r)) ->
      Halogen.HTML.HTML (Halogen.Component.ComponentSlot ps Aff act) act ->
      Maybe (r s act ps o) ->
      Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

runUI ::
  forall r f i o.
  { duplicateSlot :: Unit -> Effect Unit } ->
  RenderSpec r ->
  Halogen.Component.Component f i o Aff ->
  i ->
  Aff (Halogen.HalogenIO f o Aff)
runUI emit renderSpec component i = do
  lchs <- liftEffect newLifecycleHandlers
  disposed <- liftEffect $ Effect.Ref.new false
  Halogen.Aff.Driver.Eval.handleLifecycle lchs do
    sio <- Halogen.Subscription.create
    dsx <- Effect.Ref.read =<< runComponent lchs (liftEffect <<< Halogen.Subscription.notify sio.listener) i component
    Halogen.Aff.Driver.State.unDriverStateX
      ( \st ->
          pure
            { query: evalDriver disposed st.selfRef
            , messages: sio.emitter
            , dispose: dispose disposed lchs dsx
            }
      )
      dsx
  where
  evalDriver ::
    forall s f' act ps i' o'.
    Effect.Ref.Ref Boolean ->
    Effect.Ref.Ref (Halogen.Aff.Driver.State.DriverState r s f' act ps i' o') ->
    forall a.
    (f' a -> Aff (Maybe a))
  evalDriver disposed ref q =
    liftEffect (Effect.Ref.read disposed)
      >>=
        if _ then
          pure Nothing
        else
          Halogen.Aff.Driver.Eval.evalQ render ref q

  runComponent ::
    forall f' i' o'.
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    (o' -> Aff Unit) ->
    i' ->
    Halogen.Component f' i' o' Aff ->
    Effect (Effect.Ref.Ref (Halogen.Aff.Driver.State.DriverStateX r f' o'))
  runComponent lchs handler j =
    Halogen.Component.unComponent \c -> do
      lchs' <- newLifecycleHandlers
      var <- Halogen.Aff.Driver.State.initDriverState c j handler lchs'
      pre <- Effect.Ref.read lchs
      Effect.Ref.write { initializers: Data.List.Nil, finalizers: pre.finalizers } lchs
      Halogen.Aff.Driver.State.unDriverStateX (render lchs <<< _.selfRef) =<< Effect.Ref.read var
      squashChildInitializers lchs pre.initializers =<< Effect.Ref.read var
      pure var

  render ::
    forall s f' act ps i' o'.
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    Effect.Ref.Ref (Halogen.Aff.Driver.State.DriverState r s f' act ps i' o') ->
    Effect Unit
  render lchs var =
    Effect.Ref.read var
      >>= \(Halogen.Aff.Driver.State.DriverState ds) -> do
        shouldProcessHandlers <- Data.Maybe.isNothing <$> Effect.Ref.read ds.pendingHandlers
        when shouldProcessHandlers $ Effect.Ref.write (Just Data.List.Nil) ds.pendingHandlers
        Effect.Ref.write Halogen.Data.Slot.empty ds.childrenOut
        Effect.Ref.write ds.children ds.childrenIn
        let
          -- The following 3 defs are working around a capture bug, see #586
          pendingHandlers = identity ds.pendingHandlers

          pendingQueries = identity ds.pendingQueries

          selfRef = identity ds.selfRef

          handler :: Halogen.Query.Input.Input act -> Aff Unit
          handler = Halogen.Aff.Driver.Eval.queueOrRun pendingHandlers <<< void <<< Halogen.Aff.Driver.Eval.evalF render selfRef

          childHandler :: act -> Aff Unit
          childHandler = Halogen.Aff.Driver.Eval.queueOrRun pendingQueries <<< handler <<< Halogen.Query.Input.Action
        rendering <-
          renderSpec.render
            (Halogen.Aff.Driver.Eval.handleAff <<< handler)
            (renderChild lchs childHandler ds.childrenIn ds.childrenOut)
            (ds.component.render ds.state)
            ds.rendering
        children <- Effect.Ref.read ds.childrenOut
        childrenIn <- Effect.Ref.read ds.childrenIn
        Halogen.Data.Slot.foreachSlot childrenIn \(Halogen.Aff.Driver.State.DriverStateRef childVar) -> do
          childDS <- Effect.Ref.read childVar
          Halogen.Aff.Driver.State.renderStateX_ renderSpec.removeChild childDS
          finalize lchs childDS
        flip Effect.Ref.modify_ ds.selfRef
          $ Halogen.Aff.Driver.State.mapDriverState \ds' ->
              ds' { rendering = Just rendering, children = children }
        when shouldProcessHandlers do
          flip Control.Monad.Rec.Class.tailRecM unit \_ -> do
            handlers <- Effect.Ref.read pendingHandlers
            Effect.Ref.write (Just Data.List.Nil) pendingHandlers
            Data.Traversable.traverse_ (Halogen.Aff.Driver.Eval.handleAff <<< Data.Traversable.traverse_ Control.Monad.Fork.Class.fork <<< Data.List.reverse) handlers
            mmore <- Effect.Ref.read pendingHandlers
            if maybe false Data.List.null mmore then
              Effect.Ref.write Nothing pendingHandlers $> Control.Monad.Rec.Class.Done unit
            else
              pure $ Control.Monad.Rec.Class.Loop unit

  renderChild ::
    forall ps act.
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    (act -> Aff Unit) ->
    Effect.Ref.Ref (Halogen.Data.Slot.SlotStorage ps (Halogen.Aff.Driver.State.DriverStateRef r)) ->
    Effect.Ref.Ref (Halogen.Data.Slot.SlotStorage ps (Halogen.Aff.Driver.State.DriverStateRef r)) ->
    Halogen.Component.ComponentSlotBox ps Aff act ->
    Effect (Halogen.Aff.Driver.State.RenderStateX r)
  renderChild lchs handler childrenInRef childrenOutRef =
    Halogen.Component.unComponentSlot \slot -> do
      childrenIn <- slot.pop <$> Effect.Ref.read childrenInRef
      var <- case childrenIn of
        Just (Tuple (Halogen.Aff.Driver.State.DriverStateRef existing) childrenIn') -> do
          Effect.Ref.write childrenIn' childrenInRef
          dsx <- Effect.Ref.read existing
          Halogen.Aff.Driver.State.unDriverStateX
            ( \st -> do
                flip Effect.Ref.write st.handlerRef $ maybe (pure unit) handler <<< slot.output
                Halogen.Aff.Driver.Eval.handleAff $ Halogen.Aff.Driver.Eval.evalM render st.selfRef (st.component.eval (Halogen.Query.HalogenQ.Receive slot.input unit))
            )
            dsx
          pure existing
        Nothing -> runComponent lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
      isDuplicate <- Data.Maybe.isJust <<< slot.get <$> Effect.Ref.read childrenOutRef
      when isDuplicate do
        emit.duplicateSlot unit
      Effect.Ref.modify_ (slot.set $ Halogen.Aff.Driver.State.DriverStateRef var) childrenOutRef
      Effect.Ref.read var
        >>= Halogen.Aff.Driver.State.renderStateX case _ of
          Nothing -> Effect.Exception.throw "Halogen internal error: child was not initialized in renderChild"
          Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers ::
    forall f' o'.
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    Data.List.List (Aff Unit) ->
    Halogen.Aff.Driver.State.DriverStateX r f' o' ->
    Effect Unit
  squashChildInitializers lchs preInits =
    Halogen.Aff.Driver.State.unDriverStateX \st -> do
      let
        parentInitializer = Halogen.Aff.Driver.Eval.evalM render st.selfRef (st.component.eval (Halogen.Query.HalogenQ.Initialize unit))
      Effect.Ref.modify_
        ( \handlers ->
            { initializers:
                ( do
                    Control.Parallel.parSequence_ (Data.List.reverse handlers.initializers)
                    parentInitializer
                    liftEffect do
                      handlePending st.pendingQueries
                      handlePending st.pendingOuts
                )
                  Data.List.: preInits
            , finalizers: handlers.finalizers
            }
        )
        lchs

  finalize ::
    forall f' o'.
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    Halogen.Aff.Driver.State.DriverStateX r f' o' ->
    Effect Unit
  finalize lchs = do
    Halogen.Aff.Driver.State.unDriverStateX \st -> do
      cleanupSubscriptionsAndForks (Halogen.Aff.Driver.State.DriverState st)
      let
        f = Halogen.Aff.Driver.Eval.evalM render st.selfRef (st.component.eval (Halogen.Query.HalogenQ.Finalize unit))
      Effect.Ref.modify_
        ( \handlers ->
            { initializers: handlers.initializers
            , finalizers: f Data.List.: handlers.finalizers
            }
        )
        lchs
      Halogen.Data.Slot.foreachSlot st.children \(Halogen.Aff.Driver.State.DriverStateRef ref) -> do
        dsx <- Effect.Ref.read ref
        finalize lchs dsx

  dispose ::
    forall f' o'.
    Effect.Ref.Ref Boolean ->
    Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers ->
    Halogen.Aff.Driver.State.DriverStateX r f' o' ->
    Aff Unit
  dispose disposed lchs dsx =
    Halogen.Aff.Driver.Eval.handleLifecycle lchs do
      Effect.Ref.read disposed
        >>=
          if _ then
            pure unit
          else do
            Effect.Ref.write true disposed
            finalize lchs dsx
            dsx
              # Halogen.Aff.Driver.State.unDriverStateX \({ selfRef }) -> do
                  (Halogen.Aff.Driver.State.DriverState ds) <- liftEffect $ Effect.Ref.read selfRef
                  for_ ds.rendering renderSpec.dispose

newLifecycleHandlers :: Effect (Effect.Ref.Ref Halogen.Aff.Driver.State.LifecycleHandlers)
newLifecycleHandlers = Effect.Ref.new { initializers: Data.List.Nil, finalizers: Data.List.Nil }

handlePending :: Effect.Ref.Ref (Maybe (Data.List.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Effect.Ref.read ref
  Effect.Ref.write Nothing ref
  for_ queue (Halogen.Aff.Driver.Eval.handleAff <<< Data.Traversable.traverse_ Control.Monad.Fork.Class.fork <<< Data.List.reverse)

cleanupSubscriptionsAndForks ::
  forall r s f act ps i o.
  Halogen.Aff.Driver.State.DriverState r s f act ps i o ->
  Effect Unit
cleanupSubscriptionsAndForks (Halogen.Aff.Driver.State.DriverState ds) = do
  Data.Traversable.traverse_ (Data.Traversable.traverse_ Halogen.Subscription.unsubscribe) =<< Effect.Ref.read ds.subscriptions
  Effect.Ref.write Nothing ds.subscriptions
  Data.Traversable.traverse_ (Halogen.Aff.Driver.Eval.handleAff <<< Effect.Aff.killFiber (Effect.Aff.error "finalized")) =<< Effect.Ref.read ds.forks
  Effect.Ref.write Data.Map.empty ds.forks
