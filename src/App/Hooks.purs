module App.Hooks
  ( IndexedHookF
  , HookF
  , hook
  , component
  , Action
  , modify
  , hookEffect
  , hookAff
  , defaultOptions
  , Options
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad, iap)
import Data.Either (Either(..))
import Data.Functor.Indexed (class IxFunctor)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Meeshkan.Variant (setViaVariant)
import Prim.Row (class Lacks, class Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

hookEffect ::
  forall state action slots output proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  Effect v ->
  IndexedHookF state action slots output Effect i o v
hookEffect px v' =
  IndexedHookF
    ( \io -> do
        v <- H.liftEffect v'
        (unIndexedHookF (hook px v)) io
    )

hookAff ::
  forall state action slots output proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  Aff v ->
  IndexedHookF state action slots output Aff i o v
hookAff px v' =
  IndexedHookF
    ( \io -> do
        v <- H.liftAff v'
        (unIndexedHookF (hook px v)) io
    )

data Action :: forall k1 k2. Type -> k1 -> k2 -> Row Type -> Type
data Action input slots m o
  = Initialize
  | Modify (Variant o)
  | Receive input

modify :: forall input slots m o. Variant o -> Action input slots m o
modify = Modify

type HookHTML input slots m o
  = HC.HTML (H.ComponentSlot slots m (Action input slots m o)) (Action input slots m o)

type HookM input state action slots output m o
  = input ->
    IndexedHookF state action slots output m () o (HookHTML input slots m o)

handleAction ::
  forall input slots output m o.
  Options ->
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
handleAction options f = case _ of
  Initialize -> do
    { input } <- H.get
    ival <- runHook input (Left {})
    H.modify_ _ { hooks = Right (fst ival), html = snd ival }
  Modify v -> do
    { input, hooks } <- H.get
    let
      newHooks = case hooks of
        Left l -> Left l
        Right r -> Right (setViaVariant v r)
    o /\ html <- runHook input newHooks
    H.modify_ _ { hooks = Right o, html = html }
  Receive input ->
    when (options.receiveInput) do
      { hooks } <- H.get
      o /\ html <- runHook input hooks
      H.modify_ _ { input = input, hooks = Right o, html = html }
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

type Options
  = { receiveInput :: Boolean
    }

defaultOptions :: Options
defaultOptions = { receiveInput: false }

component ::
  forall slots o query input output m.
  Options ->
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
component o f =
  H.mkComponent
    { initialState: \input -> { input, hooks: Left {}, html: HH.div [] [] }
    , render: \{ html } -> html
    , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction o f }
    }
