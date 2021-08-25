module App.Hooks (IndexedHookF, HookF, hook, component, Action, modify) where

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
import Halogen as H
import Halogen.HTML.Core as HC
import Meeshkan.Variant (setViaVariant)
import Prim.Row (class Lacks, class Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data HookF a
  = HookF a

derive instance functorHookF :: Functor HookF

newtype IndexedHookF (i :: Row Type) (o :: Row Type) a
  = IndexedHookF (Either { | i } { | o } -> ({ | o } /\ a))

unIndexedHookF :: forall i o a. IndexedHookF i o a -> Either { | i } { | o } -> ({ | o } /\ a)
unIndexedHookF (IndexedHookF a) = a

derive instance freeHookFFunctor :: Functor (IndexedHookF i i)

instance freeHookFApply :: Apply (IndexedHookF i i) where
  apply = iapply

instance freeHookFBind :: Bind (IndexedHookF i i) where
  bind = ibind

instance freeHookFApplicative :: Applicative (IndexedHookF i i) where
  pure = ipure

instance freeHookFMonad :: Monad (IndexedHookF i i)

instance freeHookFIxFunctor :: IxFunctor IndexedHookF where
  imap f (IndexedHookF a) = IndexedHookF ((map <<< map) f a)

instance freeHookFIxApplicative :: IxApply IndexedHookF where
  iapply = iap

instance freeHookFIxApply :: IxApplicative IndexedHookF where
  ipure a =
    IndexedHookF \i ->
      ( case i of
          Left l -> l
          Right r -> r
      )
        /\ a

instance freeHookFIxBind :: IxBind IndexedHookF where
  ibind (IndexedHookF fmonad) function =
    -- we use unsafe coerce because we can only ever add hooks
    -- so the right case will guaranteed to contain the values we need
    IndexedHookF \i ->
      let
        m /\ res =
          fmonad
            ( case i of
                Left l -> Left l
                Right r -> Right (unsafeCoerce r)
            )
      in
        (unIndexedHookF (function res))
          ( case i of
              Left _ -> Left m
              Right _ -> Right (unsafeCoerce m)
          )

instance freeHookFIxMonad :: IxMonad IndexedHookF

hook ::
  forall proxy sym v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  proxy sym ->
  v ->
  IndexedHookF i o v
hook _ v =
  IndexedHookF
    ( \io ->
        ( case io of
            Left i -> Record.insert (Proxy :: _ sym) v i
            Right o -> o
        )
          /\ ( case io of
                Left _ -> v
                Right o -> Record.get (Proxy :: _ sym) o
            )
    )

data Action input slots m o
  = Initialize (HookM input slots m o)
  | Modify (Variant o)

modify :: forall input slots m o. Variant o -> Action input slots m o
modify = Modify

type HookM input slots m o
  = input ->
    IndexedHookF () o (HC.HTML (H.ComponentSlot slots m (Action input slots m o)) (Action input slots m o))

handleAction ::
  forall input o slots output m.
  Action input slots m o ->
  H.HalogenM { hooks :: Either {} { | o }, input :: input } (Action input slots m o) slots output m Unit
handleAction = case _ of
  Initialize h -> H.modify_ (\i -> i { hooks = Right (fst ((unIndexedHookF (h i.input)) (Left {}))) })
  Modify v -> do
    H.modify_
      ( \i ->
          i
            { hooks =
              case i.hooks of
                Left l -> Left l
                Right r -> Right (setViaVariant v r)
            }
      )

component ::
  forall slots o query input output m.
  HookM input slots m o ->
  H.Component query input output m
component f =
  H.mkComponent
    { initialState: \input -> { input, hooks: Left {} }
    , render: \{ input, hooks } -> snd (unIndexedHookF (f input) hooks)
    , eval: H.mkEval H.defaultEval { initialize = Just (Initialize f), handleAction = handleAction }
    }
