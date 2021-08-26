module App.Hooks
  ( IndexedHookF
  , HookF
  , hook
  , hookM
  , component
  , Action
  , set
  , setM
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class NotReadOnly
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad, iap)
import Data.Either (Either(..))
import Data.Functor.Indexed (class IxFunctor)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, inj)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Meeshkan.Variant (setViaVariant)
import Prim.Row (class Lacks, class Cons)
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

data HookF a
  = HookF a

derive instance functorHookF :: Functor HookF

newtype IndexedHookF state action slots output m (i :: Row Type) (o :: Row Type) a
  = IndexedHookF (Either { | i } { | o } -> H.HalogenM state action slots output m ({ | o } /\ a))

unIndexedHookF :: forall state action slots output m i o a. IndexedHookF state action slots output m i o a -> Either { | i } { | o } -> H.HalogenM state action slots output m ({ | o } /\ a)
unIndexedHookF (IndexedHookF a) = a

derive instance indexedHookFFunctor :: Functor (IndexedHookF state action slots output m i i)

instance indexedHookFApply :: Apply (IndexedHookF state action slots output m i i) where
  apply = iapply

instance indexedHookFBind :: Bind (IndexedHookF state action slots output m i i) where
  bind = ibind

instance indexedHookFApplicative :: Applicative (IndexedHookF state action slots output m i i) where
  pure = ipure

instance indexedHookFMonad :: Monad (IndexedHookF state action slots output m i i)

instance indexedHookFIxFunctor :: IxFunctor (IndexedHookF state action slots output m) where
  imap f (IndexedHookF a) = IndexedHookF ((map <<< map <<< map) f a)

instance indexedHookFIxApplicative :: IxApply (IndexedHookF state action slots output m) where
  iapply = iap

instance indexedHookFIxApply :: IxApplicative (IndexedHookF state action slots output m) where
  ipure a =
    IndexedHookF \i ->
      pure
        $ ( case i of
              Left l -> l
              Right r -> r
          )
        /\ a

instance indexedHookFIxBind :: IxBind (IndexedHookF state action slots output m) where
  ibind (IndexedHookF fmonad) function =
    -- we use unsafe coerce because we can only ever add hooks
    -- so the right case will guaranteed to contain the values we need
    IndexedHookF \i -> do
      m /\ res <-
        fmonad
          ( case i of
              Left l -> Left l
              Right r -> Right (unsafeCoerce r)
          )
      (unIndexedHookF (function res))
        ( case i of
            Left _ -> Left m
            Right _ -> Right (unsafeCoerce m)
        )

instance indexedHookFIxMonad :: IxMonad (IndexedHookF state action slots output m)

hook ::
  forall state action slots output m proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  v ->
  IndexedHookF state action slots output m i o v
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
  forall state action slots output proxy sym m v i o.
  Monad m =>
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  m v ->
  IndexedHookF state action slots output m i o v
hookM px v' =
  IndexedHookF
    ( \io -> do
        v <- lift v'
        (unIndexedHookF (hook px v)) io
    )

data Action :: forall k1. Type -> k1 -> (Type -> Type) -> Row Type -> Type
data Action input slots m o
  = Initialize
  | Modify (m (Variant o))
  | Receive input
  | Finalize

class NotReadOnly (a :: Type)

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

set ::
  forall proxy input slots m sym a r1 r2.
  Applicative m =>
  NotReadOnly a =>
  Cons sym a r1 r2 =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  Action input slots m r2
set px = setM px <<< pure

setM ::
  forall proxy input slots m sym a r1 r2.
  Functor m =>
  NotReadOnly a =>
  Cons sym a r1 r2 =>
  IsSymbol sym =>
  proxy sym ->
  m a ->
  Action input slots m r2
setM px v = Modify (inj px <$> v)

type HookHTML input slots m o
  = HC.HTML (H.ComponentSlot slots m (Action input slots m o)) (Action input slots m o)

type HookM input state action slots output m o
  = input ->
    IndexedHookF state action slots output m () o (HookHTML input slots m o)

handleAction ::
  forall r input slots output m o.
  Monad m =>
  { receiveInput :: Boolean, finalize :: { | o } -> m Unit | r } ->
  HookM input
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML input slots m o
    }
    (Action input slots m o)
    slots
    output
    m
    o ->
  Action input slots m o ->
  H.HalogenM
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML input slots m o
    }
    (Action input slots m o)
    slots
    output
    m
    Unit
handleAction { receiveInput, finalize } f = case _ of
  Initialize -> do
    { input } <- H.get
    ival <- runHook input (Left {})
    H.modify_ _ { hooks = Right (fst ival), html = snd ival }
  Modify v' -> do
    v <- lift v'
    { input, hooks } <- H.get
    o /\ html <-
      runHook input
        ( case hooks of
            Left l -> Left l
            Right r -> Right (setViaVariant v r)
        )
    H.modify_ _ { hooks = Right o, html = html }
  Receive input ->
    when receiveInput do
      { hooks } <- H.get
      o /\ html <- runHook input hooks
      H.modify_ _ { input = input, hooks = Right o, html = html }
  Finalize -> do
    { hooks } <- H.get
    case hooks of
      Left _ -> pure unit
      Right r -> lift (finalize r)
  where
  runHook ::
    input ->
    Either {} { | o } ->
    H.HalogenM
      { hooks :: Either {} { | o }
      , input :: input
      , html :: HookHTML input slots m o
      }
      (Action input slots m o)
      slots
      output
      m
      ({ | o } /\ HookHTML input slots m o)
  runHook input hooks = unIndexedHookF (f input) hooks

type Options :: forall k1 k2 k3 k4. Row Type -> (Type -> Type) -> k1 -> k2 -> k3 -> k4 -> (Type -> Type) -> Type
type Options o query state action slots output m
  = { receiveInput :: Boolean
    , handleQuery :: forall a. { | o } -> query a -> m (Maybe ({ | o } /\ a))
    , finalize :: { | o } -> m Unit
    }

defaultOptions ::
  forall o query state action slots output m.
  Applicative m => Options o query state action slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: \_ _ -> pure Nothing
  , finalize: \_ -> pure unit
  }

component ::
  forall slots o query input output m.
  Monad m =>
  Options o query
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML input slots m o
    }
    (Action input slots m o)
    slots
    output
    m ->
  HookM input
    { hooks :: Either {} { | o }
    , input :: input
    , html :: HookHTML input slots m o
    }
    (Action input slots m o)
    slots
    output
    m
    o ->
  H.Component query input output m
component options f =
  H.mkComponent
    { initialState: \input -> { input, hooks: Left {}, html: HH.div [] [] }
    , render: \{ html } -> html
    , eval:
        H.mkEval
          { initialize: Just Initialize
          , finalize: Just Finalize
          , receive: Just <<< Receive
          , handleAction: handleAction options f
          , handleQuery:
              \query -> do
                { hooks } <- H.get
                case hooks of
                  Left _ -> pure Nothing
                  Right o -> do
                    lifted <- lift (options.handleQuery o query)
                    case lifted of
                      Nothing -> pure Nothing
                      Just (newHooks /\ val) -> do
                        H.modify_ _ { hooks = Right newHooks }
                        pure (Just val)
          }
    }
