module App.Hooks
  ( IndexedHookF
  , HookF
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
  , Action
  , HookM
  , set
  , setM
  , setMWithHooks
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class NotReadOnly
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
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Functor.Indexed (class IxFunctor)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, inj)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (lift)
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
import Prim.Row (class Lacks, class Cons)
import Prim.Row as Row
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

newtype HookM emittedValue o input slots output m a
  = HookM
  ( H.HalogenM
      { hooks :: Either {} { | o }
      , input :: input
      , html :: HookHTML emittedValue o input slots output m
      }
      (Action emittedValue o input slots output m)
      slots
      output
      m
      a
  )

derive newtype instance functorHookM :: Functor (HookM emittedValue o input slots output m)

derive newtype instance applyHookM :: Apply (HookM emittedValue o input slots output m)

derive newtype instance applicativeHookM :: Applicative (HookM emittedValue o input slots output m)

derive newtype instance bindHookM :: Bind (HookM emittedValue o input slots output m)

derive newtype instance monadHookM :: Monad (HookM emittedValue o input slots output m)

derive newtype instance semigroupHookM :: Semigroup a => Semigroup (HookM emittedValue o input slots output m a)

derive newtype instance monoidHookM :: Monoid a => Monoid (HookM emittedValue o input slots output m a)

derive newtype instance monadEffectHookM :: MonadEffect m => MonadEffect (HookM emittedValue o input slots output m)

derive newtype instance monadAffHookM :: MonadAff m => MonadAff (HookM emittedValue o input slots output m)

instance parallelHookM :: Parallel (HookAp emittedValue o input slots output m) (HookM emittedValue o input slots output m) where
  parallel (HookM a) = HookAp (parallel a)
  sequential (HookAp a) = HookM (sequential a)

instance monadTransHookM :: MonadTrans (HookM emittedValue o input slots output) where
  lift = HookM <<< lift

derive newtype instance monadRecHookM :: MonadRec (HookM emittedValue o input slots output m)

derive newtype instance monadAskHookM :: MonadAsk r m => MonadAsk r (HookM emittedValue o input slots output m)

derive newtype instance monadTellHookM :: MonadTell w m => MonadTell w (HookM emittedValue o input slots output m)

derive newtype instance monadThrowHookM :: MonadThrow e m => MonadThrow e (HookM emittedValue o input slots output m)

-- | An applicative-only version of `HalogenM` to allow for parallel evaluation.
newtype HookAp emittedValue o input slots output m a
  = HookAp
  ( HalogenAp
      { hooks :: Either {} { | o }
      , input :: input
      , html :: HookHTML emittedValue o input slots output m
      }
      (Action emittedValue o input slots output m)
      slots
      output
      m
      a
  )

derive instance newtypeHookAp :: Newtype (HookAp emittedValue o input slots output m a) _

derive newtype instance functorHookAp :: Functor (HookAp emittedValue o input slots output m)

derive newtype instance applyHookAp :: Apply (HookAp emittedValue o input slots output m)

derive newtype instance applicativeHookAp :: Applicative (HookAp emittedValue o input slots output m)

-- | Raises an output message for the component.
raise :: forall emittedValue o input slots output m. output -> HookM emittedValue o input slots output m Unit
raise o = HookM (H.raise o)

-- | Sends a query to a child of a component at the specified slot.
query ::
  forall emittedValue o input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  slot ->
  query a ->
  HookM emittedValue o input slots output m (Maybe a)
query label p q = HookM (H.query label p q)

-- | Sends a query to all children of a component at a given slot label.
queryAll ::
  forall emittedValue o input output m label slots query output' slot a _1.
  Row.Cons label (Slot query output' slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  Proxy label ->
  query a ->
  HookM emittedValue o input slots output m (Map slot a)
queryAll label q = HookM (H.queryAll label q)

subscribe :: forall o input emittedValue slots output m. HS.Emitter emittedValue -> HookM emittedValue o input slots output m H.SubscriptionId
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
subscribe' :: forall emittedValue o input slots output m. (H.SubscriptionId -> HS.Emitter emittedValue) -> HookM emittedValue o input slots output m Unit
subscribe' esc = HookM (H.subscribe' ((map <<< map) Emit esc))

-- | Unsubscribes a component from a subscription. If the subscription associated
-- | with the ID has already ended this will have no effect.
unsubscribe :: forall emittedValue o input slots output m. H.SubscriptionId -> HookM emittedValue o input slots output m Unit
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
fork :: forall emittedValue o input slots output m. HookM emittedValue o input slots output m Unit -> HookM emittedValue o input slots output m H.ForkId
fork (HookM hmu) = HookM (H.fork hmu)

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall emittedValue o input slots output m. H.ForkId -> HookM emittedValue o input slots output m Unit
kill fid = HookM (H.kill fid)

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall emittedValue o input slots output m. RefLabel -> HookM emittedValue o input slots output m (Maybe Element)
getRef p = HookM (H.getRef p)

mapOutput ::
  forall emittedValue o input slots output output' m.
  (output -> output') ->
  HookM emittedValue o input slots output m
    ~> HookM emittedValue o input slots output' m
mapOutput f (HookM h) = HookM (unsafeCoerce $ HM.mapOutput f h)

hoist ::
  forall emittedValue o input slots output m m'.
  Functor m' =>
  (m ~> m') ->
  HookM emittedValue o input slots output m
    ~> HookM emittedValue o input slots output m'
hoist nat (HookM fa) = HookM (unsafeCoerce $ HM.hoist nat fa)

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

data HookF a
  = HookF a

derive instance functorHookF :: Functor HookF

newtype IndexedHookF emittedValue input slots output m (i :: Row Type) (o :: Row Type) a
  = IndexedHookF
  ( Either { | i } { | o } ->
    H.HalogenM
      { hooks :: Either {} { | o }
      , input :: input
      , html :: HookHTML emittedValue o input slots output m
      }
      (Action emittedValue o input slots output m)
      slots
      output
      m
      ({ | o } /\ a)
  )

unIndexedHookF ::
  forall emittedValue input slots output m i o a.
  IndexedHookF emittedValue input slots output m i o a ->
  Either { | i } { | o } ->
  H.HalogenM
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML emittedValue o input slots output m
    }
    (Action emittedValue o input slots output m)
    slots
    output
    m
    ({ | o } /\ a)
unIndexedHookF (IndexedHookF a) = a

derive instance indexedHookFFunctor :: Functor (IndexedHookF emittedValue input slots output m i i)

instance indexedHookFApply :: Apply (IndexedHookF emittedValue input slots output m i i) where
  apply = iapply

instance indexedHookFBind :: Bind (IndexedHookF emittedValue input slots output m i i) where
  bind = ibind

instance indexedHookFApplicative :: Applicative (IndexedHookF emittedValue input slots output m i i) where
  pure = ipure

instance indexedHookFMonad :: Monad (IndexedHookF emittedValue input slots output m i i)

instance indexedHookFIxFunctor :: IxFunctor (IndexedHookF emittedValue input slots output m) where
  imap f (IndexedHookF a) = IndexedHookF ((map <<< map <<< map) f a)

instance indexedHookFIxApplicative :: IxApply (IndexedHookF emittedValue input slots output m) where
  iapply = iap

instance indexedHookFIxApply :: IxApplicative (IndexedHookF emittedValue input slots output m) where
  ipure a =
    IndexedHookF \i ->
      pure
        $ ( case i of
              Left l -> l
              Right r -> r
          )
        /\ a

instance indexedHookFIxBind :: IxBind (IndexedHookF emittedValue input slots output m) where
  ibind (IndexedHookF fmonad) function =
    -- we use unsafe coerce because we can only ever add hooks
    -- so the right case will guaranteed to contain the values we need
    unsafeCoerce
      $ IndexedHookF \i -> do
          m /\ res <-
            fmonad
              ( case i of
                  Left l -> Left l
                  Right r -> Right (unsafeCoerce r)
              )
          unsafeCoerce
            $ (unIndexedHookF (function res))
                ( case i of
                    Left _ -> Left m
                    Right _ -> Right (unsafeCoerce m)
                )

instance indexedHookFIxMonad :: IxMonad (IndexedHookF emittedValue input slots output m)

hook ::
  forall emittedValue input slots output m proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  v ->
  IndexedHookF emittedValue input slots output m i o v
hook _ v =
  IndexedHookF
    ( \io ->
        pure
          $ ( case io of
                Left i -> Record.insert (Proxy :: _ sym) v i
                Right o -> o
            )
          /\ ( case io of
                Left _ -> v
                Right o -> Record.get (Proxy :: _ sym) o
            )
    )

hookM ::
  forall emittedValue input slots output proxy sym m v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  HookM emittedValue o input slots output m v ->
  IndexedHookF emittedValue input slots output m i o v
hookM px (HookM v') =
  IndexedHookF
    ( \io -> do
        v <- v'
        (unIndexedHookF (hook px v)) io
    )

data Action emittedValue o input slots output m
  = Initialize
  | Emit emittedValue
  | Modify (HookM emittedValue o input slots output m { | o } -> HookM emittedValue o input slots output m (Variant o))
  | Receive input
  | Finalize

class NotReadOnly (a :: Type)

unsafeHooks ::
  forall emittedValue o input slots output m.
  HookM emittedValue o input slots output m { | o }
unsafeHooks =
  HookM do
    { hooks } <- H.get
    pure (unsafePartial (fromJust (hush hooks)))

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

set ::
  forall proxy emittedValue input output slots m sym a r1 o.
  NotReadOnly a =>
  Cons sym a r1 o =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  Action emittedValue o input slots output m
set px a = setM px (pure a)

setM ::
  forall proxy emittedValue output input slots m sym a r1 o.
  NotReadOnly a =>
  Cons sym a r1 o =>
  IsSymbol sym =>
  proxy sym ->
  HookM emittedValue o input slots output m a ->
  Action emittedValue o input slots output m
setM px v = setMWithHooks px (pure v)

setMWithHooks ::
  forall proxy emittedValue output input slots m sym a r1 o.
  NotReadOnly a =>
  Cons sym a r1 o =>
  IsSymbol sym =>
  proxy sym ->
  (HookM emittedValue o input slots output m { | o } -> HookM emittedValue o input slots output m a) ->
  Action emittedValue o input slots output m
setMWithHooks px v = Modify ((map <<< map) (inj px) v)

type HookHTML emittedValue o input slots output m
  = HC.HTML (H.ComponentSlot slots m (Action emittedValue o input slots output m)) (Action emittedValue o input slots output m)

type HookArg emittedValue input slots output m o
  = input ->
    IndexedHookF emittedValue input slots output m () o (HookHTML emittedValue o input slots output m)

handleAction ::
  forall r emittedValue input slots output m o.
  { finalize :: { | o } -> HookM emittedValue o input slots output m Unit
  , handleEmittedValue :: { | o } -> emittedValue -> HookM emittedValue o input slots output m (Maybe { | o })
  | r
  } ->
  HookArg emittedValue input slots output m o ->
  Action emittedValue o input slots output m ->
  H.HalogenM
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML emittedValue o input slots output m
    }
    (Action emittedValue o input slots output m)
    slots
    output
    m
    Unit
handleAction { handleEmittedValue, finalize } f = case _ of
  Initialize -> do
    { input } <- H.get
    hooks /\ html <- runHook input (Left {})
    H.modify_ _ { hooks = Right hooks, html = html }
  Modify v' -> do
    v <- let HookM v'' = v' unsafeHooks in v''
    { input, hooks } <- H.get
    o /\ html <-
      runHook input
        ( case hooks of
            Left l -> Left l
            Right r -> Right (setViaVariant v r)
        )
    H.modify_ _ { hooks = Right o, html = html }
  Receive input -> do
    { hooks } <- H.get
    o /\ html <- runHook input hooks
    H.modify_ _ { input = input, hooks = Right o, html = html }
  Finalize -> do
    { hooks } <- H.get
    case hooks of
      Left _ -> pure unit
      Right r -> let HookM done = finalize r in done
  Emit emittedValue -> do
    { hooks } <- H.get
    case hooks of
      Left _ -> pure unit
      Right r ->
        let
          HookM newHooks = handleEmittedValue r emittedValue
        in
          newHooks >>= flip for_ \newHooks' -> H.modify_ _ { hooks = Right newHooks' }
  where
  runHook ::
    input ->
    Either {} { | o } ->
    H.HalogenM
      { hooks :: Either {} { | o }
      , input :: input
      , html :: HookHTML emittedValue o input slots output m
      }
      (Action emittedValue o input slots output m)
      slots
      output
      m
      ({ | o } /\ HookHTML emittedValue o input slots output m)
  runHook input hooks = unIndexedHookF (f input) hooks

type Options query emittedValue o input slots output m
  = { receiveInput :: Boolean
    , handleQuery :: forall a. { | o } -> query a -> HookM emittedValue o input slots output m (Maybe { | o } /\ Maybe a)
    , handleEmittedValue :: { | o } -> emittedValue -> HookM emittedValue o input slots output m (Maybe { | o })
    , finalize :: { | o } -> HookM emittedValue o input slots output m Unit
    }

defaultOptions ::
  forall query emittedValue o input slots output m.
  Options query emittedValue o input slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: \_ _ -> pure (Nothing /\ Nothing)
  , handleEmittedValue: \_ _ -> pure Nothing
  , finalize: \_ -> pure unit
  }

component ::
  forall emittedValue slots o query input output m.
  Options query emittedValue o input slots output m ->
  HookArg emittedValue input slots output m o ->
  H.Component query input output m
component options f =
  H.mkComponent
    { initialState: \input -> { input, hooks: Left {}, html: HH.div [] [] }
    , render: \{ html } -> html
    , eval:
        H.mkEval
          { initialize: Just Initialize
          , finalize: Just Finalize
          , receive: if options.receiveInput then Just <<< Receive else const Nothing
          , handleAction: handleAction options f
          , handleQuery:
              \q -> do
                { hooks } <- H.get
                case hooks of
                  Left _ -> pure Nothing
                  Right o -> do
                    newHooks /\ val <- let HookM res = options.handleQuery o q in res
                    for_ newHooks \newHooks' -> H.modify_ _ { hooks = Right newHooks' }
                    pure val
          }
    }
