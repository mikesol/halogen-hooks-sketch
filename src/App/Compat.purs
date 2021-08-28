module App.Hooks.Compat where

import Prelude
import App.Hooks as Hooks
import App.Sugar as Sugar
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Bind.Indexed (class IxBind, class IxDiscard, ibind, idiscard, imap)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Prelude as Applicative
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))

data QueryToken :: forall k. k -> Type
data QueryToken query
  = QueryToken

data SlotsToken :: forall k. k -> Type
data SlotsToken slots
  = SlotsToken

data OutputToken :: forall k. k -> Type
data OutputToken output
  = OutputToken

type ComponentTokens :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
type ComponentTokens q s o
  = { queryToken :: QueryToken q
    , slotsToken :: SlotsToken s
    , outputToken :: OutputToken o
    }

component ::
  forall slots hooks query input output m.
  (ComponentTokens query slots output -> Hooks.HookArg hooks input slots output m) ->
  H.Component query input output m
component f =
  Hooks.component Hooks.defaultOptions
    ( f
        { queryToken: QueryToken
        , slotsToken: SlotsToken
        , outputToken: OutputToken
        }
    )

useState ::
  forall hooks' hooks input slots output sym sym' m v i iRL o.
  RL.RowToList i iRL =>
  Sugar.GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Cons sym v hooks' hooks =>
  v ->
  Hooks.IndexedHookM hooks input slots output m i o (v /\ Proxy sym)
useState = imap (flip (/\) (Proxy :: _ sym)) <<< Hooks.hook (Proxy :: _ sym) <<< Applicative.pure

pure :: ∀ m a x. IxApplicative m ⇒ a → m x x a
pure = ipure

bind :: ∀ a b m x y z. IxBind m ⇒ m x y a → (a → m y z b) → m x z b
bind = ibind

discard ∷ ∀ a k f b (x :: k) (y :: k) (z :: k). IxDiscard a ⇒ IxBind f ⇒ f x y a → (a → f y z b) → f x z b
discard = idiscard

modify_ ::
  forall proxy output input slots m sym a r1 hooks.
  Hooks.NotReadOnly a =>
  Row.Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  (a -> a) ->
  Hooks.HookAction hooks input slots output m
modify_ px f =
  Hooks.doThis do
    Hooks.getHookCons px <$> Hooks.getHooksM
      >>= case _ of
          Nothing -> Applicative.pure unit
          Just v -> Hooks.setHookMCons px (f v)
