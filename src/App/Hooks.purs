module App.Hooks
  ( IndexedHookM
  , hook
  , hookM
  , raise
  , query
  , subscribe
  , subscribe'
  , unsubscribe
  , queryAll
  , fork
  , kill
  , getRef
  , mapOutput
  , hoist
  , component
  , capture
  , getHooks
  , lift
  , Action
  , HookM
  , set
  , setM
  , setMWithHooks
  , setHook
  , unhedgeAt
  , setHedgedAt
  , Hedged
  , modify
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class GetLexicalLast
  , class NotReadOnly
  , HookHTML
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, ipure, iwhen)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Indexed (class IxMonad, iap)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Functor.Indexed (class IxFunctor)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, inj)
import Data.Variant.Internal (VariantRep)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Data.Slot (Slot)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenM (HalogenAp(..))
import Halogen.Query.HalogenM as HM
import Halogen.Query.Input (RefLabel)
import Halogen.Subscription as HS
import Prim.Row (class Cons, class Lacks)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

foreign import unsafeSetViaVariant :: forall a b. VariantRep a -> { | b } -> { | b }

setViaVariant :: forall row. Variant row -> { | row } -> { | row }
setViaVariant = unsafeSetViaVariant <<< coerceV
  where
  coerceV :: forall a. Variant row -> VariantRep a
  coerceV = unsafeCoerce

setHedgedViaVariant :: forall row. Variant row -> Hedged row -> Hedged row
setHedgedViaVariant v = unsafeUnhedge >>> setViaVariant v >>> hedge

newtype HookM hooks emittedValue input slots output m a
  = HookM
  ( H.HalogenM
      { hooks :: (Hedged hooks)
      , input :: input
      , html :: HookHTML hooks emittedValue input slots output m
      }
      (Action hooks emittedValue input slots output m)
      slots
      output
      m
      a
  )

derive newtype instance functorHookM :: Functor (HookM hooks emittedValue input slots output m)

derive newtype instance applyHookM :: Apply (HookM hooks emittedValue input slots output m)

derive newtype instance applicativeHookM :: Applicative (HookM hooks emittedValue input slots output m)

derive newtype instance bindHookM :: Bind (HookM hooks emittedValue input slots output m)

derive newtype instance monadHookM :: Monad (HookM hooks emittedValue input slots output m)

derive newtype instance semigroupHookM :: Semigroup a => Semigroup (HookM hooks emittedValue input slots output m a)

derive newtype instance monoidHookM :: Monoid a => Monoid (HookM hooks emittedValue input slots output m a)

derive newtype instance monadEffectHookM :: MonadEffect m => MonadEffect (HookM hooks emittedValue input slots output m)

derive newtype instance monadAffHookM :: MonadAff m => MonadAff (HookM hooks emittedValue input slots output m)

instance parallelHookM :: Parallel (HookAp hooks emittedValue input slots output m) (HookM hooks emittedValue input slots output m) where
  parallel (HookM a) = HookAp (parallel a)
  sequential (HookAp a) = HookM (sequential a)

instance monadTransHookM :: MonadTrans (HookM hooks emittedValue input slots output) where
  lift = HookM <<< H.lift

derive newtype instance monadRecHookM :: MonadRec (HookM hooks emittedValue input slots output m)

derive newtype instance monadAskHookM :: MonadAsk r m => MonadAsk r (HookM hooks emittedValue input slots output m)

derive newtype instance monadTellHookM :: MonadTell w m => MonadTell w (HookM hooks emittedValue input slots output m)

derive newtype instance monadThrowHookM :: MonadThrow e m => MonadThrow e (HookM hooks emittedValue input slots output m)

-- | An applicative-only version of `HalogenM` to allow for parallel evaluation.
newtype HookAp hooks emittedValue input slots output m a
  = HookAp
  ( HalogenAp
      { hooks :: (Hedged hooks)
      , input :: input
      , html :: HookHTML hooks emittedValue input slots output m
      }
      (Action hooks emittedValue input slots output m)
      slots
      output
      m
      a
  )

derive instance newtypeHookAp :: Newtype (HookAp hooks emittedValue input slots output m a) _

derive newtype instance functorHookAp :: Functor (HookAp hooks emittedValue input slots output m)

derive newtype instance applyHookAp :: Apply (HookAp hooks emittedValue input slots output m)

derive newtype instance applicativeHookAp :: Applicative (HookAp hooks emittedValue input slots output m)

-- | Raises an output message for the component.
raise :: forall hooks emittedValue input slots output m. output -> HookM hooks emittedValue input slots output m Unit
raise o = HookM (H.raise o)

-- | Sends a query to a child of a component at the specified slot.
query ::
  forall hooks emittedValue input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  slot ->
  query a ->
  HookM hooks emittedValue input slots output m (Maybe a)
query label p q = HookM (H.query label p q)

-- | Sends a query to all children of a component at a given slot label.
queryAll ::
  forall hooks emittedValue input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  query a ->
  HookM hooks emittedValue input slots output m (Map slot a)
queryAll label q = HookM (H.queryAll label q)

subscribe :: forall hooks emittedValue input slots output m. HS.Emitter emittedValue -> HookM hooks emittedValue input slots output m H.SubscriptionId
subscribe es = HookM (H.subscribe (map Emit es))

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `Emitter` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe' :: forall hooks emittedValue input slots output m. (H.SubscriptionId -> HS.Emitter emittedValue) -> HookM hooks emittedValue input slots output m Unit
subscribe' esc = HookM (H.subscribe' ((map <<< map) Emit esc))

-- | Unsubscribes a component from a subscription. If the subscription associated
-- | with the ID has already ended this will have no effect.
unsubscribe :: forall hooks emittedValue input slots output m. H.SubscriptionId -> HookM hooks emittedValue input slots output m Unit
unsubscribe sid = HookM (H.unsubscribe sid)

-- | Starts a `HalogenM` process running independent from the current `eval`
-- | "thread".
-- |
-- | A commonly use case for `fork` is in component initializers where some
-- | async action is started. Normally all interaction with the component will
-- | be blocked until the initializer completes, but if the async action is
-- | `fork`ed instead, the initializer can complete synchronously while the
-- | async action continues.
-- |
-- | Some care needs to be taken when using a `fork` that can modify the
-- | component state, as it's easy for the forked process to "clobber" the state
-- | (overwrite some or all of it with an old value) by mistake.
-- |
-- | When a component is disposed of any active forks will automatically
-- | be killed. New forks can be started during finalization but there will be
-- | no means of killing them.
fork :: forall hooks emittedValue input slots output m. HookM hooks emittedValue input slots output m Unit -> HookM hooks emittedValue input slots output m H.ForkId
fork (HookM hmu) = HookM (H.fork hmu)

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall hooks emittedValue input slots output m. H.ForkId -> HookM hooks emittedValue input slots output m Unit
kill fid = HookM (H.kill fid)

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall hooks emittedValue input slots output m. RefLabel -> HookM hooks emittedValue input slots output m (Maybe Element)
getRef p = HookM (H.getRef p)

mapOutput ::
  forall hooks emittedValue input slots output output' m.
  (output -> output') ->
  HookM hooks emittedValue input slots output m
    ~> HookM hooks emittedValue input slots output' m
mapOutput f (HookM h) = HookM (unsafeCoerce $ HM.mapOutput f h)

hoist ::
  forall hooks emittedValue input slots output m m'.
  Functor m' =>
  (m ~> m') ->
  HookM hooks emittedValue input slots output m
    ~> HookM hooks emittedValue input slots output m'
hoist nat (HookM fa) = HookM (unsafeCoerce $ HM.hoist nat fa)

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

newtype IndexedHookM hooks emittedValue input slots output m (i :: Row Type) (o :: Row Type) a
  = IndexedHookM (HookM hooks emittedValue input slots output m a)

derive instance indexedHookFFunctor :: Functor (IndexedHookM hooks emittedValue input slots output m i i)

instance indexedHookFApply :: Apply (IndexedHookM hooks emittedValue input slots output m i i) where
  apply = iapply

instance indexedHookFBind :: Bind (IndexedHookM hooks emittedValue input slots output m i i) where
  bind = ibind

instance indexedHookFApplicative :: Applicative (IndexedHookM hooks emittedValue input slots output m i i) where
  pure = ipure

instance indexedHookFMonad :: Monad (IndexedHookM hooks emittedValue input slots output m i i)

instance indexedHookFIxFunctor :: IxFunctor (IndexedHookM hooks emittedValue input slots output m) where
  imap f (IndexedHookM a) = IndexedHookM (map f a)

instance indexedHookFIxApply :: IxApply (IndexedHookM hooks emittedValue input slots output m) where
  iapply = iap

instance indexedHookFIxApplicative :: IxApplicative (IndexedHookM hooks emittedValue input slots output m) where
  ipure = IndexedHookM <<< pure

instance indexedHookFIxBind :: IxBind (IndexedHookM hooks emittedValue input slots output m) where
  ibind (IndexedHookM fmonad) function = IndexedHookM (fmonad >>= \f -> let IndexedHookM o = function f in o)

instance indexedHookFIxMonad :: IxMonad (IndexedHookM hooks emittedValue input slots output m)

lift ::
  forall hooks emittedValue input slots output m v i.
  HookM hooks emittedValue input slots output m v ->
  IndexedHookM hooks emittedValue input slots output m i i v
lift = IndexedHookM

class GetLexicalLast (default :: Symbol) (i :: RL.RowList Type) (s :: Symbol) | default i -> s

instance getLexicalLastNil :: GetLexicalLast sym RL.Nil sym

instance getLexicalLastCons :: GetLexicalLast sym rest o => GetLexicalLast prev (RL.Cons sym val rest) o

capture ::
  forall iRL sym' hooks' hooks emittedValue input slots output m sym v i o.
  RL.RowToList i iRL =>
  GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Cons sym v hooks' hooks =>
  Eq v =>
  v ->
  HookM hooks emittedValue input slots output m Unit ->
  IndexedHookM hooks emittedValue input slots output m i o Unit
capture v m = Ix.do
  prev <- hook px v
  iwhen (prev /= v) (lift (setHook px v *> m))
  where
  px = Proxy :: _ sym

hook ::
  forall hooks' hooks emittedValue input slots output proxy sym m v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Cons sym v hooks' hooks =>
  proxy sym ->
  v ->
  IndexedHookM hooks emittedValue input slots output m i o v
hook px = hookM px <<< pure

hookM ::
  forall hooks' hooks emittedValue input slots output proxy sym m v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Cons sym v hooks' hooks =>
  proxy sym ->
  HookM hooks emittedValue input slots output m v ->
  IndexedHookM hooks emittedValue input slots output m i o v
hookM px m = IndexedHookM go
  where
  go =
    unhedgeAt px <$> getHooks
      >>= case _ of
          Nothing -> (m >>= setHook px) *> go
          Just v -> pure v

data Action hooks emittedValue input slots output m
  = Initialize
  | Emit emittedValue
  | Modify
    ( HookM hooks emittedValue input slots output m { | hooks } ->
      HookM hooks emittedValue input slots output m (Variant hooks)
    )
  | Receive input
  | Finalize

unsafeHooks ::
  forall hooks emittedValue input slots output m.
  HookM hooks emittedValue input slots output m { | hooks }
unsafeHooks =
  HookM do
    { hooks } <- H.get
    pure (unsafeUnhedge hooks)

setHook ::
  forall proxy emittedValue output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  HookM hooks emittedValue input slots output m Unit
setHook px a =
  HookM
    ( H.modify_ \i ->
        i { hooks = setHedgedAt px a i.hooks }
    )

data Hedged (r :: Row Type)

foreign import unhedgeAtFFI :: forall a r. Maybe a -> (a -> Maybe a) -> String -> Hedged r -> Maybe a

unhedgeAt ::
  forall proxy sym a r1 hooks.
  IsSymbol sym =>
  Cons sym a r1 hooks =>
  proxy sym ->
  Hedged hooks ->
  Maybe a
unhedgeAt = unhedgeAtFFI Nothing Just <<< reflectSymbol

foreign import setHedgedAtFFI :: forall a r. String -> a -> Hedged r -> Hedged r

setHedgedAt ::
  forall proxy sym a r1 hooks.
  IsSymbol sym =>
  Cons sym a r1 hooks =>
  proxy sym ->
  a ->
  Hedged hooks ->
  Hedged hooks
setHedgedAt = setHedgedAtFFI <<< reflectSymbol

unsafeUnhedge :: forall r. Hedged r -> { | r }
unsafeUnhedge = unsafeCoerce

hedge :: forall r. { | r } -> Hedged r
hedge = unsafeCoerce

getHooks ::
  forall hooks emittedValue input slots output m.
  HookM hooks emittedValue input slots output m (Hedged hooks)
getHooks = HookM (H.gets _.hooks)

class NotReadOnly (a :: Type)

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

set ::
  forall proxy emittedValue input output slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  Action hooks emittedValue input slots output m
set px a = setM px (pure a)

setM ::
  forall proxy emittedValue output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  HookM hooks emittedValue input slots output m a ->
  Action hooks emittedValue input slots output m
setM px v = setMWithHooks px (pure v)

setMWithHooks ::
  forall proxy emittedValue output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  ( HookM hooks emittedValue input slots output m { | hooks } ->
    HookM hooks emittedValue input slots output m a
  ) ->
  Action hooks emittedValue input slots output m
setMWithHooks px v = Modify ((map <<< map) (inj px) v)

modify ::
  forall proxy emittedValue output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  (a -> a) ->
  Action hooks emittedValue input slots output m
modify px f = setMWithHooks px (map f <<< map (Record.get (Proxy :: _ sym)))

type HookHTML hooks emittedValue input slots output m
  = HC.HTML (H.ComponentSlot slots m (Action hooks emittedValue input slots output m)) (Action hooks emittedValue input slots output m)

type HookArg hooks emittedValue input slots output m
  = input ->
    IndexedHookM hooks emittedValue input slots output m () hooks (HookHTML hooks emittedValue input slots output m)

handleAction ::
  forall hooks emittedValue input slots output m rest.
  { finalize :: HookM hooks emittedValue input slots output m Unit
  , handleEmittedValue :: emittedValue -> HookM hooks emittedValue input slots output m Unit
  | rest
  } ->
  HookArg hooks emittedValue input slots output m ->
  Action hooks emittedValue input slots output m ->
  H.HalogenM
    { hooks :: (Hedged hooks)
    , input :: input
    , html :: HookHTML hooks emittedValue input slots output m
    }
    (Action hooks emittedValue input slots output m)
    slots
    output
    m
    Unit
handleAction { handleEmittedValue, finalize } f = case _ of
  Initialize -> do
    { input } <- H.get
    html <- let IndexedHookM (HookM m) = f input in m
    H.modify_ _ { html = html }
  Modify v' -> do
    v <- let HookM v'' = v' unsafeHooks in v''
    H.modify_ \i -> i { hooks = setHedgedViaVariant v i.hooks }
    { input } <- H.get
    html <- let IndexedHookM (HookM m) = f input in m
    H.modify_ _ { html = html }
  Receive i -> do
    H.modify_ _ { input = i }
    { input } <- H.get
    html <- let IndexedHookM (HookM m) = f input in m
    H.modify_ _ { html = html }
  Finalize -> let HookM done = finalize in done
  Emit emittedValue -> let HookM handled = handleEmittedValue emittedValue in handled

type Options query hooks emittedValue input slots output m
  = { receiveInput :: Boolean
    , handleQuery :: forall a. query a -> HookM hooks emittedValue input slots output m (Maybe a)
    , handleEmittedValue :: emittedValue -> HookM hooks emittedValue input slots output m Unit
    , finalize :: HookM hooks emittedValue input slots output m Unit
    , initialHTML :: HookHTML hooks emittedValue input slots output m
    }

defaultOptions ::
  forall query hooks emittedValue input slots output m.
  Options query hooks emittedValue input slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: \_ -> pure Nothing
  , handleEmittedValue: \_ -> pure unit
  , finalize: pure unit
  , initialHTML: HH.div [] []
  }

component ::
  forall emittedValue slots hooks query input output m.
  Options query hooks emittedValue input slots output m ->
  HookArg hooks emittedValue input slots output m ->
  H.Component query input output m
component options f =
  H.mkComponent
    { initialState: \input -> { input, hooks: unsafeCoerce {}, html: options.initialHTML }
    , render: \{ html } -> html
    , eval:
        H.mkEval
          { initialize: Just Initialize
          , finalize: Just Finalize
          , receive: if options.receiveInput then Just <<< Receive else const Nothing
          , handleAction: handleAction options f
          , handleQuery:
              \q -> let HookM res = options.handleQuery q in res
          }
    }
