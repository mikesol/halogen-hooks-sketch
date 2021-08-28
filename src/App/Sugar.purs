module App.Sugar where

import Prelude

import App.Hooks (class NotReadOnly, HookAction, HookM, IndexedHookM, doThis, hook, lift, setHookMCons)
import Control.Applicative.Indexed (iwhen)
import Control.Monad.Indexed.Qualified as Ix
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))

class GetLexicalLast (default :: Symbol) (i :: RL.RowList Type) (s :: Symbol) | default i -> s

instance getLexicalLastNil :: GetLexicalLast sym RL.Nil sym

instance getLexicalLastCons :: GetLexicalLast sym rest o => GetLexicalLast prev (RL.Cons sym val rest) o

capture ::
  forall iRL sym' hooks' hooks input slots output m sym v i o.
  RL.RowToList i iRL =>
  GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Cons sym v hooks' hooks =>
  Eq v =>
  v ->
  HookM hooks input slots output m Unit ->
  IndexedHookM hooks input slots output m i o Unit
capture v m = Ix.do
  prev <- hook px (pure v)
  iwhen (prev /= v) (lift (setHookMCons px v *> m))
  where
  px = Proxy :: _ sym

useState ::
  forall hooks' hooks input slots output proxy sym m v i o.
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Cons sym v hooks' hooks =>
  proxy sym ->
  v ->
  IndexedHookM hooks input slots output m i o v
useState px = hook px <<< pure

setM ::
  forall proxy output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Row.Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  HookM hooks input slots output m a ->
  HookAction hooks input slots output m
setM px v = doThis (v >>= (void <<< setHookMCons px))

set ::
  forall proxy output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Row.Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  HookAction hooks input slots output m
set px = setM px <<< pure