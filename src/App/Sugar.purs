module App.Sugar where

import Prelude

import App.Hooks (class NotReadOnly, HookAction, HookM, IndexedHookM, doThis, hookCons, lift, setHookMCons)
import Control.Applicative.Indexed (iwhen)
import Control.Monad.Indexed.Qualified as Ix
import Data.Symbol (class IsSymbol)
import Prim.Row as Row

capture ::
  forall proxy hooks' hooks input slots output m sym v i o.
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Lacks sym hooks' =>
  Row.Cons sym v hooks' hooks =>
  Eq v =>
  proxy sym ->
  v ->
  HookM hooks input slots output m Unit ->
  IndexedHookM hooks input slots output m i o Unit
capture px v m = Ix.do
  prev <- hookCons px (pure v)
  iwhen (prev /= v) (lift (setHookMCons px v *> m))

useState ::
  forall hooks' hooks input slots output proxy sym m v i o.
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Lacks sym hooks' =>
  Row.Cons sym v hooks' hooks =>
  proxy sym ->
  v ->
  IndexedHookM hooks input slots output m i o v
useState px = hookCons px <<< pure

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