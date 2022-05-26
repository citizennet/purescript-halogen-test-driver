-- | A Halogen driver for automated tests.
-- |
-- | It's a trim-down version of `Halogen.VDom.Driver` which only cares about
-- | component states and effects with all DOM interactions (adding/removing
-- | DOM nodes, updating node properties, attaching/removing event handlers,
-- | etc.) removed from the original code.
module Halogen.Test.Driver
  ( ChildRenderer
  , RenderState(..)
  , VHTML
  , runUI
  ) where

import Pre

import Data.Array as Data.Array
import Data.Newtype as Data.Newtype
import Data.Tuple as Data.Tuple
import Effect.Ref as Effect.Ref
import Effect.Uncurried as Effect.Uncurried
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.Aff.Driver.State as Halogen.Aff.Driver.State
import Halogen.Component as Halogen.Component
import Halogen.HTML.Core as Halogen.HTML.Core
import Halogen.Query.Input as Halogen.Query.Input
import Halogen.Test.Driver.Aff as Halogen.Test.Driver.Aff
import Halogen.VDom as Halogen.VDom
import Halogen.VDom.Thunk as Halogen.VDom.Thunk
import Halogen.VDom.Util as Halogen.VDom.Util

-- | Copied from `Halogen.VDom.Driver` which is not exported
type VHTML action slots =
  Halogen.VDom.VDom
    (Array (Halogen.HTML.Core.Prop (Halogen.Query.Input.Input action)))
    (Halogen.Component.ComponentSlot slots Aff action)

-- | Adapted from `Halogen.VDom.Driver.ChildRenderer`
-- | * replaced `RenderState` by our own definition
type ChildRenderer action slots =
  Halogen.Component.ComponentSlotBox slots Aff action ->
  Effect (Halogen.Aff.Driver.State.RenderStateX RenderState)

-- | Adapted from `Halogen.VDom.Driver.RenderState`
-- | * removed `node :: Web.DOM.Node.Node`
-- | * set machine output to `Unit` in place of `Web.DOM.Node.Node`
newtype RenderState state action slots output =
  RenderState
    { machine :: Halogen.VDom.Step (VHTML action slots) Unit
    , renderChildRef :: Effect.Ref.Ref (ChildRenderer action slots)
    }

-- | Adapted from `Halogen.VDom.Driver.runUI`
-- | * we reuse `Halogen.Aff.Driver.runUI` since it's all about component
-- |   state, effects (State, Subscribe, Unsubscribe, Lift, ChildQuery,
-- |   Raise, Par, Fork, Kill, GetRef) and life cycle (Initialize,
-- |   Finalize).
-- | * replace `renderSpec` by our own implementation
runUI ::
  forall query input output.
  { duplicateSlot :: Unit -> Effect Unit } ->
  Halogen.Component.Component query input output Aff ->
  input ->
  Aff (Halogen.HalogenIO query output Aff)
runUI emit component i = do
  Halogen.Test.Driver.Aff.runUI emit renderSpec component i

-- | Adapted from `Halogen.VDom.Driver.renderSpec`
-- | * replace `render` by our own implementation
-- | * remove DOM effects in `removeChild` and `dispose`
renderSpec :: Halogen.Test.Driver.Aff.RenderSpec RenderState
renderSpec =
  { render
  , renderChild: identity
  , removeChild: \_ -> pure unit
  , dispose: \_ -> pure unit
  }

-- | Adapted from `Halogen.VDom.Driver.renderSpec.render`
-- | * `handler` is no longer used since we don't build attributes
render ::
  forall state action slots output.
  (Halogen.Query.Input.Input action -> Effect Unit) ->
  ( Halogen.Component.ComponentSlotBox slots Aff action ->
    Effect (Halogen.Aff.Driver.State.RenderStateX RenderState)
  ) ->
  Halogen.HTML.Core.HTML
    (Halogen.Component.ComponentSlot slots Aff action)
    action ->
  Maybe (RenderState state action slots output) ->
  Effect (RenderState state action slots output)
render _handler renderChild (Halogen.HTML.Core.HTML vdom) = case _ of
  Nothing -> do
    renderChildRef <- Effect.Ref.new renderChild
    machine <- Effect.Uncurried.runEffectFn1 (buildVDom renderChildRef) vdom
    pure $ RenderState { machine, renderChildRef }
  Just (RenderState { machine, renderChildRef }) -> do
    Effect.Ref.write renderChild renderChildRef
    machine' <- Effect.Uncurried.runEffectFn2 Halogen.VDom.step machine vdom
    pure $ RenderState { machine: machine', renderChildRef }

-- | Adapted from `Halogen.VDom.DOM.VDomMachine`
-- | * set machine output to `Unit` in place of `Web.DOM.Node.Node`
-- | * specialize `Halogen.VDom.VDom a w` to `VHTML action slots`
type VDomMachine action slots =
  Halogen.VDom.Machine (VHTML action slots) Unit

-- | Adapted from `Halogen.VDom.DOM.VDomStep`
-- | * set machine output to `Unit` in place of `Web.DOM.Node.Node`
-- | * specialize `Halogen.VDom.VDom a w` to `VHTML action slots`
type VDomStep action slots =
  Halogen.VDom.Step (VHTML action slots) Unit

-- | Adapted from `Halogen.VDom.DOM.buildVDom`
buildVDom ::
  forall action slots.
  Effect.Ref.Ref (ChildRenderer action slots) ->
  VDomMachine action slots
buildVDom renderChildRef = build
  where
  build :: VDomMachine action slots
  build =
    Effect.Uncurried.mkEffectFn1 case _ of
      Halogen.VDom.Text _ -> Effect.Uncurried.runEffectFn1 buildText build
      Halogen.VDom.Elem _ _ _ children -> Effect.Uncurried.runEffectFn1 buildElem { build, children }
      Halogen.VDom.Keyed _ _ _ children -> Effect.Uncurried.runEffectFn1 buildKeyed { build, children }
      Halogen.VDom.Widget slot -> Effect.Uncurried.runEffectFn1 buildWidget { build, renderChildRef, slot }
      Halogen.VDom.Grafted g -> do
        Effect.Uncurried.runEffectFn1 build (Halogen.VDom.runGraft g)

-- | Adapted from `Halogen.VDom.DOM.TextState`
-- | * removed `node :: Web.DOM.Node.Node`
-- | * removed `value :: String`
-- |   * we don't need to compare and patch in DOM
type TextState a w =
  { build :: VDomMachine a w
  }

-- | Adapted from `Halogen.VDom.DOM.buildText`
buildText ::
  forall a w.
  Effect.Uncurried.EffectFn1
    (VDomMachine a w)
    (VDomStep a w)
buildText =
  Effect.Uncurried.mkEffectFn1 \build -> do
    let
      state :: TextState a w
      state = { build }
    pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit state patchText haltText

-- | Adapted from `Halogen.VDom.DOM.patchText`
patchText ::
  forall a w.
  Effect.Uncurried.EffectFn2
    (TextState a w)
    (VHTML a w)
    (VDomStep a w)
patchText =
  Effect.Uncurried.mkEffectFn2 \state vdom -> do
    let
      { build } = state
    case vdom of
      Halogen.VDom.Grafted g -> do
        Effect.Uncurried.runEffectFn2 patchText state (Halogen.VDom.runGraft g)
      Halogen.VDom.Text _ -> do
        pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit state patchText haltText
      _ -> do
        Effect.Uncurried.runEffectFn1 haltText state
        Effect.Uncurried.runEffectFn1 build vdom

-- | Adapted from `Halogen.VDom.DOM.haltText`
haltText :: forall a w. Effect.Uncurried.EffectFn1 (TextState a w) Unit
haltText =
  Effect.Uncurried.mkEffectFn1 \_ -> do
    pure unit

-- | Adapted from `Halogen.VDom.DOM.ElemState`
-- | * removed `node :: Web.DOM.Node.Node`
-- | * removed `attrs ∷ Halogen.VDom.Step a Unit`
-- |   * we don't need to register event handlers on DOM
-- | * removed `ns` and `name`
-- |   * we don't need to compare and patch in DOM
type ElemState a w =
  { build :: VDomMachine a w
  , children :: Array (VDomStep a w)
  }

-- | Adapted from `Halogen.VDom.DOM.buildElem`
buildElem ::
  forall a w.
  Effect.Uncurried.EffectFn1
    { build :: VDomMachine a w
    , children :: Array (VHTML a w)
    }
    (VDomStep a w)
buildElem =
  Effect.Uncurried.mkEffectFn1 \({ build, children: ch1 }) -> do
    let
      onChild =
        Effect.Uncurried.mkEffectFn2 \_ix child -> do
          Effect.Uncurried.runEffectFn1 build child
    children <- Effect.Uncurried.runEffectFn2 Halogen.VDom.Util.forE ch1 onChild
    let
      state :: ElemState a w
      state =
        { build
        , children
        }
    pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit state patchElem haltElem

-- | Adapted from `Halogen.VDom.DOM.patchElem`
patchElem ::
  forall a w.
  Effect.Uncurried.EffectFn2
    (ElemState a w)
    (VHTML a w)
    (VDomStep a w)
patchElem =
  Effect.Uncurried.mkEffectFn2 \state vdom -> do
    let
      { build, children: ch1 } = state
    case vdom of
      Halogen.VDom.Grafted g -> do
        Effect.Uncurried.runEffectFn2 patchElem state (Halogen.VDom.runGraft g)
      Halogen.VDom.Elem _ns2 _name2 _as2 ch2 -> do
        case Data.Array.length ch1, Data.Array.length ch2 of
          0, 0 -> do
            let
              nextState :: ElemState a w
              nextState =
                { build
                , children: ch1
                }
            pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit nextState patchElem haltElem
          _, _ -> do
            let
              onThese =
                Effect.Uncurried.mkEffectFn3 \_ix s v -> do
                  Effect.Uncurried.runEffectFn2 Halogen.VDom.step s v

              onThis =
                Effect.Uncurried.mkEffectFn2 \_ s -> do
                  Effect.Uncurried.runEffectFn1 Halogen.VDom.halt s

              onThat =
                Effect.Uncurried.mkEffectFn2 \_ix v -> do
                  Effect.Uncurried.runEffectFn1 build v
            children2 <- Effect.Uncurried.runEffectFn5 Halogen.VDom.Util.diffWithIxE ch1 ch2 onThese onThis onThat
            let
              nextState =
                { build
                , children: children2
                }
            pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit nextState patchElem haltElem
      _ -> do
        Effect.Uncurried.runEffectFn1 haltElem state
        Effect.Uncurried.runEffectFn1 build vdom

-- | Adapted from `Halogen.VDom.DOM.haltElem`
haltElem :: forall a w. Effect.Uncurried.EffectFn1 (ElemState a w) Unit
haltElem =
  Effect.Uncurried.mkEffectFn1 \({ children }) -> do
    Effect.Uncurried.runEffectFn2 Halogen.VDom.Util.forEachE children
      Halogen.VDom.halt

-- | Adapted from `Halogen.VDom.DOM.KeyedState`
-- | * removed `node :: Web.DOM.Node.Node`
-- | * removed `attrs ∷ Halogen.VDom.Step a Unit`
-- |   * we don't need to register event handlers on DOM
-- | * removed `ns` and `name`
-- |   * we don't need to compare and patch in DOM
type KeyedState a w =
  { build :: VDomMachine a w
  , children :: Foreign.Object.Object (VDomStep a w)
  , length :: Int
  }

-- | Adapted from `Halogen.VDom.DOM.buildKeyed`
buildKeyed ::
  forall a w.
  Effect.Uncurried.EffectFn1
    { build :: VDomMachine a w
    , children :: Array (Tuple String (VHTML a w))
    }
    (VDomStep a w)
buildKeyed =
  Effect.Uncurried.mkEffectFn1 \({ build, children: ch1 }) -> do
    let
      onChild =
        Effect.Uncurried.mkEffectFn3 \_ _ix (Tuple _ vdom) -> do
          Effect.Uncurried.runEffectFn1 build vdom
    children <- Effect.Uncurried.runEffectFn3 Halogen.VDom.Util.strMapWithIxE ch1 Data.Tuple.fst onChild
    let
      state =
        { build
        , children
        , length: Data.Array.length ch1
        }
    pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit state patchKeyed haltKeyed

-- | Adapted from `Halogen.VDom.DOM.patchKeyed`
patchKeyed ::
  forall a w.
  Effect.Uncurried.EffectFn2
    (KeyedState a w)
    (VHTML a w)
    (VDomStep a w)
patchKeyed =
  Effect.Uncurried.mkEffectFn2 \state vdom -> do
    let
      { build, children: ch1, length: len1 } = state
    case vdom of
      Halogen.VDom.Grafted g -> do
        Effect.Uncurried.runEffectFn2 patchKeyed state (Halogen.VDom.runGraft g)
      Halogen.VDom.Keyed _ns2 _name2 _as2 ch2 -> case len1, Data.Array.length ch2 of
        0, 0 -> do
          let
            nextState =
              { build
              , children: ch1
              , length: 0
              }
          pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit nextState patchKeyed haltKeyed
        _, len2 -> do
          let
            onThese =
              Effect.Uncurried.mkEffectFn4 \_ _ix' s (Tuple _ v) -> do
                Effect.Uncurried.runEffectFn2 Halogen.VDom.step s v

            onThis =
              Effect.Uncurried.mkEffectFn2 \_ s -> do
                Effect.Uncurried.runEffectFn1 Halogen.VDom.halt s

            onThat =
              Effect.Uncurried.mkEffectFn3 \_ _ix (Tuple _ v) -> do
                Effect.Uncurried.runEffectFn1 build v
          children2 <- Effect.Uncurried.runEffectFn6 Halogen.VDom.Util.diffWithKeyAndIxE ch1 ch2 Data.Tuple.fst onThese onThis onThat
          let
            nextState =
              { build
              , children: children2
              , length: len2
              }
          pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit nextState patchKeyed haltKeyed
      _ -> do
        Effect.Uncurried.runEffectFn1 haltKeyed state
        Effect.Uncurried.runEffectFn1 build vdom

-- | Adapted from `Halogen.VDom.DOM.haltKeyed`
haltKeyed :: forall a w. Effect.Uncurried.EffectFn1 (KeyedState a w) Unit
haltKeyed =
  Effect.Uncurried.mkEffectFn1 \({ children }) -> do
    Effect.Uncurried.runEffectFn2 Halogen.VDom.Util.forInE children do
      Effect.Uncurried.mkEffectFn2 \_ s -> do
        Effect.Uncurried.runEffectFn1 Halogen.VDom.halt s

-- | Combined `Halogen.VDom.Driver.WidgetState` and `Halogen.VDom.DOM.WidgetState`
type WidgetState action slots =
  { build :: VDomMachine action slots
  , renderChildRef :: Effect.Ref.Ref (ChildRenderer action slots)
  , widget :: Maybe (Halogen.VDom.Step (HTMLThunk action slots) Unit)
  }

-- | Copied from Halogen.VDom.Driver which is not exported
type HTMLThunk action slots =
  Halogen.VDom.Thunk.Thunk
    (Halogen.HTML.Core.HTML (Halogen.Component.ComponentSlot slots Aff action))
    action

-- | Adapted from `Halogen.VDom.Driver.mkSpec.buildWidget.render`
buildWidget ::
  forall action slots.
  Effect.Uncurried.EffectFn1
    { build :: VDomMachine action slots
    , renderChildRef :: Effect.Ref.Ref (ChildRenderer action slots)
    , slot :: Halogen.Component.ComponentSlot slots Aff action
    }
    (VDomStep action slots)
buildWidget =
  Effect.Uncurried.mkEffectFn1 \({ build, renderChildRef, slot }) -> case slot of
    Halogen.Component.ComponentSlot cs -> do
      Effect.Uncurried.runEffectFn2 renderComponentSlot { build, renderChildRef, widget: Nothing } cs
    Halogen.Component.ThunkSlot t -> do
      step <- Effect.Uncurried.runEffectFn1 (buildThunk build Data.Newtype.unwrap) t
      pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step (Halogen.VDom.extract step) { build, renderChildRef, widget: Just step } patchWidget haltWidget

-- | Adapted from `Halogen.VDom.Driver.mkSpec.buildWidget.patch`
patchWidget ::
  forall a w.
  Effect.Uncurried.EffectFn2
    (WidgetState a w)
    (VHTML a w)
    (VDomStep a w)
patchWidget =
  Effect.Uncurried.mkEffectFn2 \state vdom -> case vdom of
    Halogen.VDom.Grafted g -> do
      Effect.Uncurried.runEffectFn2 patchWidget state (Halogen.VDom.runGraft g)
    Halogen.VDom.Widget slot
      | Just step <- state.widget -> case slot of
          Halogen.Component.ComponentSlot cs -> do
            Effect.Uncurried.runEffectFn1 Halogen.VDom.halt step
            Effect.Uncurried.runEffectFn2 renderComponentSlot state cs
          Halogen.Component.ThunkSlot t -> do
            step' <- Effect.Uncurried.runEffectFn2 Halogen.VDom.step step t
            pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step (Halogen.VDom.extract step') (state { widget = Just step' }) patchWidget haltWidget
    _ -> do
      Effect.Uncurried.runEffectFn1 haltWidget state
      Effect.Uncurried.runEffectFn1 state.build vdom

-- | Adapted from `Halogen.VDom.Driver.mkSpec.buildWidget.renderComponentSlot`
renderComponentSlot ::
  forall action slots.
  Effect.Uncurried.EffectFn2
    (WidgetState action slots)
    (Halogen.Component.ComponentSlotBox slots Aff action)
    (VDomStep action slots)
renderComponentSlot =
  Effect.Uncurried.mkEffectFn2 \state cs -> do
    renderChild <- Effect.Ref.read state.renderChildRef
    _rsx <- renderChild cs
    pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step unit (state { widget = Nothing }) patchWidget haltWidget

-- | Adapted from `Halogen.VDom.Driver.mkSpec.done`
haltWidget :: forall a w. Effect.Uncurried.EffectFn1 (WidgetState a w) Unit
haltWidget =
  Effect.Uncurried.mkEffectFn1 \st -> case st.widget of
    Just step -> Effect.Uncurried.runEffectFn1 Halogen.VDom.halt step
    _ -> pure unit

-- | Adapted from `Halogen.VDom.DOM.ThunkState`
-- | * set machine output to `Unit` in place of `Web.DOM.Node.Node`
type ThunkState (f :: Type -> Type) i a w =
  { thunk :: Halogen.VDom.Thunk.Thunk f i
  , vdom :: Halogen.VDom.Step (VHTML a w) Unit
  }

-- | Adapted from `Halogen.VDom.Thunk.buildThunk`
buildThunk ::
  forall f i a w.
  VDomMachine a w ->
  (f i -> VHTML a w) ->
  Halogen.VDom.Machine (Halogen.VDom.Thunk.Thunk f i) Unit
buildThunk build toVDom = renderThunk
  where
  renderThunk :: Halogen.VDom.Machine (Halogen.VDom.Thunk.Thunk f i) Unit
  renderThunk =
    Effect.Uncurried.mkEffectFn1 \t -> do
      vdom <- Effect.Uncurried.runEffectFn1 build (toVDom (Halogen.VDom.Thunk.runThunk t))
      pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step (Halogen.VDom.extract vdom) { thunk: t, vdom } patchThunk haltThunk

  patchThunk :: Effect.Uncurried.EffectFn2 (ThunkState f i a w) (Halogen.VDom.Thunk.Thunk f i) (Halogen.VDom.Step (Halogen.VDom.Thunk.Thunk f i) Unit)
  patchThunk =
    Effect.Uncurried.mkEffectFn2 \state t2 -> do
      let
        { vdom: prev } = state
      -- NOTE `unsafeEqThunk` along with many types are not exported
      -- so the equality check is removed from here for now
      --
      -- if Fn.runFn2 unsafeEqThunk t1 t2
      --   then pure $ M.mkStep $ M.Step (M.extract prev) state patchThunk haltThunk
      --   else do
      vdom <- Effect.Uncurried.runEffectFn2 Halogen.VDom.step prev (toVDom (Halogen.VDom.Thunk.runThunk t2))
      pure $ Halogen.VDom.mkStep $ Halogen.VDom.Step (Halogen.VDom.extract vdom) { vdom, thunk: t2 } patchThunk haltThunk

  haltThunk :: Effect.Uncurried.EffectFn1 (ThunkState f i a w) Unit
  haltThunk =
    Effect.Uncurried.mkEffectFn1 \state -> do
      Effect.Uncurried.runEffectFn1 Halogen.VDom.halt state.vdom
