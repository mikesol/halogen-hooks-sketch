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
  , getHooks
  , lift
  , Action
  , HookM
  , set
  , setM
  , setMWithHooks
  , setHook
  , modify
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class NotReadOnly
  , class HedgeHooks
  , class HedgeHooksRL
  , unsafeUnhedgeHoods
  , unsafeUnhedgeHoodsRL
  , HookHTML
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Indexed (class IxMonad, iap)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Functor.Indexed (class IxFunctor)
import Data.Lens as Lens
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
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
import Meeshkan.Variant (setViaVariant)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

newtype HookM emittedValue hedgedHooks hooks input slots output m a
  = HookM
  ( H.HalogenM
      { hooks :: { | hedgedHooks }
      , input :: input
      , html :: HookHTML emittedValue hedgedHooks hooks input slots output m
      }
      (Action emittedValue hedgedHooks hooks input slots output m)
      slots
      output
      m
      a
  )

derive newtype instance functorHookM :: Functor (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance applyHookM :: Apply (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance applicativeHookM :: Applicative (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance bindHookM :: Bind (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance monadHookM :: Monad (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance semigroupHookM :: Semigroup a => Semigroup (HookM emittedValue hedgedHooks hooks input slots output m a)

derive newtype instance monoidHookM :: Monoid a => Monoid (HookM emittedValue hedgedHooks hooks input slots output m a)

derive newtype instance monadEffectHookM :: MonadEffect m => MonadEffect (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance monadAffHookM :: MonadAff m => MonadAff (HookM emittedValue hedgedHooks hooks input slots output m)

instance parallelHookM :: Parallel (HookAp emittedValue hedgedHooks hooks input slots output m) (HookM emittedValue hedgedHooks hooks input slots output m) where
  parallel (HookM a) = HookAp (parallel a)
  sequential (HookAp a) = HookM (sequential a)

instance monadTransHookM :: MonadTrans (HookM emittedValue hedgedHooks hooks input slots output) where
  lift = HookM <<< H.lift

derive newtype instance monadRecHookM :: MonadRec (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance monadAskHookM :: MonadAsk r m => MonadAsk r (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance monadTellHookM :: MonadTell w m => MonadTell w (HookM emittedValue hedgedHooks hooks input slots output m)

derive newtype instance monadThrowHookM :: MonadThrow e m => MonadThrow e (HookM emittedValue hedgedHooks hooks input slots output m)

-- | An applicative-only version of `HalogenM` to allow for parallel evaluation.
newtype HookAp emittedValue hedgedHooks hooks input slots output m a
  = HookAp
  ( HalogenAp
      { hooks :: { | hedgedHooks }
      , input :: input
      , html :: HookHTML emittedValue hedgedHooks hooks input slots output m
      }
      (Action emittedValue hedgedHooks hooks input slots output m)
      slots
      output
      m
      a
  )

derive instance newtypeHookAp :: Newtype (HookAp emittedValue hedgedHooks hooks input slots output m a) _

derive newtype instance functorHookAp :: Functor (HookAp emittedValue hedgedHooks hooks input slots output m)

derive newtype instance applyHookAp :: Apply (HookAp emittedValue hedgedHooks hooks input slots output m)

derive newtype instance applicativeHookAp :: Applicative (HookAp emittedValue hedgedHooks hooks input slots output m)

-- | Raises an output message for the component.
raise :: forall emittedValue hedgedHooks hooks input slots output m. output -> HookM emittedValue hedgedHooks hooks input slots output m Unit
raise o = HookM (H.raise o)

-- | Sends a query to a child of a component at the specified slot.
query ::
  forall emittedValue hedgedHooks hooks input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  slot ->
  query a ->
  HookM emittedValue hedgedHooks hooks input slots output m (Maybe a)
query label p q = HookM (H.query label p q)

-- | Sends a query to all children of a component at a given slot label.
queryAll ::
  forall emittedValue hedgedHooks hooks input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  query a ->
  HookM emittedValue hedgedHooks hooks input slots output m (Map slot a)
queryAll label q = HookM (H.queryAll label q)

subscribe :: forall emittedValue hedgedHooks hooks input slots output m. HS.Emitter emittedValue -> HookM emittedValue hedgedHooks hooks input slots output m H.SubscriptionId
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
subscribe' :: forall emittedValue hedgedHooks hooks input slots output m. (H.SubscriptionId -> HS.Emitter emittedValue) -> HookM emittedValue hedgedHooks hooks input slots output m Unit
subscribe' esc = HookM (H.subscribe' ((map <<< map) Emit esc))

-- | Unsubscribes a component from a subscription. If the subscription associated
-- | with the ID has already ended this will have no effect.
unsubscribe :: forall emittedValue hedgedHooks hooks input slots output m. H.SubscriptionId -> HookM emittedValue hedgedHooks hooks input slots output m Unit
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
fork :: forall emittedValue hedgedHooks hooks input slots output m. HookM emittedValue hedgedHooks hooks input slots output m Unit -> HookM emittedValue hedgedHooks hooks input slots output m H.ForkId
fork (HookM hmu) = HookM (H.fork hmu)

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall emittedValue hedgedHooks hooks input slots output m. H.ForkId -> HookM emittedValue hedgedHooks hooks input slots output m Unit
kill fid = HookM (H.kill fid)

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall emittedValue hedgedHooks hooks input slots output m. RefLabel -> HookM emittedValue hedgedHooks hooks input slots output m (Maybe Element)
getRef p = HookM (H.getRef p)

mapOutput ::
  forall emittedValue hedgedHooks hooks input slots output output' m.
  (output -> output') ->
  HookM emittedValue hedgedHooks hooks input slots output m
    ~> HookM emittedValue hedgedHooks hooks input slots output' m
mapOutput f (HookM h) = HookM (unsafeCoerce $ HM.mapOutput f h)

hoist ::
  forall emittedValue hedgedHooks hooks input slots output m m'.
  Functor m' =>
  (m ~> m') ->
  HookM emittedValue hedgedHooks hooks input slots output m
    ~> HookM emittedValue hedgedHooks hooks input slots output m'
hoist nat (HookM fa) = HookM (unsafeCoerce $ HM.hoist nat fa)

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

newtype IndexedHookM emittedValue hedgedHooks hooks input slots output m (i :: Row Type) (o :: Row Type) a
  = IndexedHookM (HookM emittedValue hedgedHooks hooks input slots output m a)

derive instance indexedHookFFunctor :: Functor (IndexedHookM emittedValue hedgedHooks hooks input slots output m i i)

instance indexedHookFApply :: Apply (IndexedHookM emittedValue hedgedHooks hooks input slots output m i i) where
  apply = iapply

instance indexedHookFBind :: Bind (IndexedHookM emittedValue hedgedHooks hooks input slots output m i i) where
  bind = ibind

instance indexedHookFApplicative :: Applicative (IndexedHookM emittedValue hedgedHooks hooks input slots output m i i) where
  pure = ipure

instance indexedHookFMonad :: Monad (IndexedHookM emittedValue hedgedHooks hooks input slots output m i i)

instance indexedHookFIxFunctor :: IxFunctor (IndexedHookM emittedValue hedgedHooks hooks input slots output m) where
  imap f (IndexedHookM a) = IndexedHookM (map f a)

instance indexedHookFIxApply :: IxApply (IndexedHookM emittedValue hedgedHooks hooks input slots output m) where
  iapply = iap

instance indexedHookFIxApplicative :: IxApplicative (IndexedHookM emittedValue hedgedHooks hooks input slots output m) where
  ipure = IndexedHookM <<< pure

instance indexedHookFIxBind :: IxBind (IndexedHookM emittedValue hedgedHooks hooks input slots output m) where
  ibind (IndexedHookM fmonad) function = IndexedHookM (fmonad >>= \f -> let IndexedHookM o = function f in o)

instance indexedHookFIxMonad :: IxMonad (IndexedHookM emittedValue hedgedHooks hooks input slots output m)

lift ::
  forall emittedValue hedgedHooks hooks input slots output m v i.
  HookM emittedValue hedgedHooks hooks input slots output m v ->
  IndexedHookM emittedValue hedgedHooks hooks input slots output m i i v
lift = IndexedHookM

hook ::
  forall hedgedHooks' emittedValue hedgedHooks hooks input slots output m proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Cons sym (Maybe (First (v))) hedgedHooks' hedgedHooks =>
  proxy sym ->
  v ->
  IndexedHookM emittedValue hedgedHooks hooks input slots output m i o v
hook px = hookM px <<< pure

hookM ::
  forall hedgedHooks' emittedValue hedgedHooks hooks input slots output proxy sym m v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Cons sym (Maybe (First (v))) hedgedHooks' hedgedHooks =>
  proxy sym ->
  HookM emittedValue hedgedHooks hooks input slots output m v ->
  IndexedHookM emittedValue hedgedHooks hooks input slots output m i o v
hookM px m =
  IndexedHookM do
    { hooks } <- HookM H.get
    case Record.get px hooks of
      Nothing ->
        (m >>= setHook px)
          *> ( let
                IndexedHookM ihf =
                  ( hookM ::
                      proxy sym ->
                      HookM emittedValue hedgedHooks hooks input slots output m v ->
                      IndexedHookM emittedValue hedgedHooks hooks input slots output m i o v
                  )
                    px
                    m
              in
                ihf
            )
      Just (First v) -> pure v

data Action emittedValue hedgedHooks hooks input slots output m
  = Initialize
  | Emit emittedValue
  | Modify
    ( HookM emittedValue hedgedHooks hooks input slots output m { | hooks } ->
      HookM emittedValue hedgedHooks hooks input slots output m (Variant hedgedHooks)
    )
  | Receive input
  | Finalize

class HedgeHooksRL (hedgedHooksRL :: RL.RowList Type) (hooksRL :: RL.RowList Type) hedgedHooks hooks | hedgedHooksRL -> hooksRL hedgedHooks hooks, hooksRL -> hedgedHooksRL hedgedHooks hooks where
  unsafeUnhedgeHoodsRL :: forall proxy. Partial => proxy hedgedHooksRL -> proxy hooksRL -> { | hedgedHooks } -> { | hooks }

instance unsafeUnhedgeHoodsRLNil :: HedgeHooksRL RL.Nil RL.Nil () () where
  unsafeUnhedgeHoodsRL _ _ _ = {}

instance unsafeUnhedgeHoodsRLCons ::
  ( IsSymbol key
  , Lacks key hooks'
  , Lacks key hedgedHooks'
  , Cons key (Maybe (First val)) hedgedHooks' hedgedHooks
  , Cons key val hooks' hooks
  , HedgeHooksRL hedgedRest rest hedgedHooks' hooks'
  , Cons key val hooks' hooks
  ) =>
  HedgeHooksRL (RL.Cons key (Maybe (First val)) hedgedRest) (RL.Cons key val rest) hedgedHooks hooks where
  unsafeUnhedgeHoodsRL _ _ o =
    Record.insert (Proxy :: _ key)
      (unwrap (fromJust (Record.get (Proxy :: _ key) o)))
      (unsafeUnhedgeHoodsRL (Proxy :: _ hedgedRest) (Proxy :: _ rest) (Record.delete (Proxy :: _ key) o))

class HedgeHooks hedgedHooks hooks | hedgedHooks -> hooks, hooks -> hedgedHooks where
  unsafeUnhedgeHoods :: Partial => { | hedgedHooks } -> { | hooks }

instance unsafeUnhedgeHoodsAll :: (RowToList hedgedHooks hedgedHooksRL, RowToList hooks hooksRL, HedgeHooksRL hedgedHooksRL hooksRL hedgedHooks hooks) => HedgeHooks hedgedHooks hooks where
  unsafeUnhedgeHoods = unsafeUnhedgeHoodsRL (Proxy :: _ hedgedHooksRL) (Proxy :: _ hooksRL)

unsafeHooks ::
  forall emittedValue hedgedHooks hooks input slots output m.
  HedgeHooks hedgedHooks hooks =>
  HookM emittedValue hedgedHooks hooks input slots output m { | hooks }
unsafeHooks =
  HookM do
    { hooks } <- H.get
    pure (unsafePartial (unsafeUnhedgeHoods hooks))

setHook ::
  forall proxy emittedValue output input slots m sym a r1 hedgedHooks hooks.
  NotReadOnly a =>
  Cons sym (Maybe (First a)) r1 hedgedHooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  HookM emittedValue hedgedHooks hooks input slots output m Unit
setHook px a =
  HookM
    ( H.modify_ \i ->
        i { hooks = Lens.set (prop px) (Just (First a)) i.hooks }
    )

getHooks ::
  forall emittedValue hedgedHooks hooks input slots output m.
  HookM emittedValue hedgedHooks hooks input slots output m { | hedgedHooks }
getHooks = HookM (H.gets _.hooks)

class NotReadOnly (a :: Type)

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

set ::
  forall proxy emittedValue input output slots m sym a r1 hedgedHooks hooks.
  NotReadOnly a =>
  Cons sym (Maybe (First a)) r1 hedgedHooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  Action emittedValue hedgedHooks hooks input slots output m
set px a = setM px (pure a)

setM ::
  forall proxy emittedValue output input slots m sym a r1 hedgedHooks hooks.
  NotReadOnly a =>
  Cons sym (Maybe (First a)) r1 hedgedHooks =>
  IsSymbol sym =>
  proxy sym ->
  HookM emittedValue hedgedHooks hooks input slots output m a ->
  Action emittedValue hedgedHooks hooks input slots output m
setM px v = setMWithHooks px (pure v)

setMWithHooks ::
  forall proxy emittedValue output input slots m sym a r1 hedgedHooks hooks.
  NotReadOnly a =>
  Cons sym (Maybe (First a)) r1 hedgedHooks =>
  IsSymbol sym =>
  proxy sym ->
  ( HookM emittedValue hedgedHooks hooks input slots output m { | hooks } ->
    HookM emittedValue hedgedHooks hooks input slots output m a
  ) ->
  Action emittedValue hedgedHooks hooks input slots output m
setMWithHooks px v = Modify ((map <<< map) (inj px <<< Just <<< First) v)

modify ::
  forall proxy emittedValue output input slots m sym a r1 hedgedHooks hooks.
  NotReadOnly a =>
  Cons sym (Maybe (First a)) r1 hedgedHooks =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  (a -> a) ->
  Action emittedValue hedgedHooks hooks input slots output m
modify px f = setMWithHooks px (map f <<< map (Record.get (Proxy :: _ sym)))

type HookHTML emittedValue hedgedHooks hooks input slots output m
  = HC.HTML (H.ComponentSlot slots m (Action emittedValue hedgedHooks hooks input slots output m)) (Action emittedValue hedgedHooks hooks input slots output m)

type HookArg emittedValue hedgedHooks hooks input slots output m
  = input ->
    IndexedHookM emittedValue hedgedHooks hooks input slots output m () hooks (HookHTML emittedValue hedgedHooks hooks input slots output m)

handleAction ::
  forall hedgedHooks hooks emittedValue input slots output m rest.
  { finalize :: HookM emittedValue hedgedHooks hooks input slots output m Unit
  , handleEmittedValue :: emittedValue -> HookM emittedValue hedgedHooks hooks input slots output m Unit
  | rest
  } ->
  HedgeHooks hedgedHooks hooks =>
  HookArg emittedValue hedgedHooks hooks input slots output m ->
  Action emittedValue hedgedHooks hooks input slots output m ->
  H.HalogenM
    { hooks :: { | hedgedHooks }
    , input :: input
    , html :: HookHTML emittedValue hedgedHooks hooks input slots output m
    }
    (Action emittedValue hedgedHooks hooks input slots output m)
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
    H.modify_ \i -> i { hooks = setViaVariant v i.hooks }
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

type Options query emittedValue hedgedHooks hooks input slots output m
  = { receiveInput :: Boolean
    , handleQuery :: forall a. query a -> HookM emittedValue hedgedHooks hooks input slots output m (Maybe a)
    , handleEmittedValue :: emittedValue -> HookM emittedValue hedgedHooks hooks input slots output m Unit
    , finalize :: HookM emittedValue hedgedHooks hooks input slots output m Unit
    , initialHTML :: HookHTML emittedValue hedgedHooks hooks input slots output m
    }

defaultOptions ::
  forall query emittedValue hedgedHooks hooks input slots output m.
  Options query emittedValue hedgedHooks hooks input slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: \_ -> pure Nothing
  , handleEmittedValue: \_ -> pure unit
  , finalize: pure unit
  , initialHTML: HH.div [] []
  }

component ::
  forall emittedValue slots hedgedHooks hooks query input output m.
  HedgeHooks hedgedHooks hooks =>
  Monoid { | hedgedHooks } =>
  Options query emittedValue hedgedHooks hooks input slots output m ->
  HookArg emittedValue hedgedHooks hooks input slots output m ->
  H.Component query input output m
component options f =
  H.mkComponent
    { initialState: \input -> { input, hooks: mempty, html: options.initialHTML }
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
