module App.Hooks.Compat where

import Prelude

import App.Hooks (getHookCons)
import App.Hooks as Hooks
import App.Sugar as Sugar
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Bind.Indexed (class IxBind, class IxDiscard, ibind, idiscard, imap)
import Control.Monad.Indexed.Qualified as Ix
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Prelude as Applicative
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data QueryToken (query :: Type -> Type)
  = QueryToken

data SlotsToken (slots :: Row Type)
  = SlotsToken

data OutputToken (output :: Type)
  = OutputToken

type ComponentTokens :: (Type -> Type) -> Row Type -> Type -> Type
type ComponentTokens q s o
  = { queryToken :: QueryToken q
    , slotsToken :: SlotsToken s
    , outputToken :: OutputToken o
    }

component ::
  forall (query :: Type -> Type) (hooks' :: Row Type) (input :: Type) (slots :: Row Type) (output :: Type) (m :: Type -> Type).
  Row.Lacks "" hooks' =>
  ( ComponentTokens query slots output ->
    input ->
    Hooks.IndexedHookM
      ( "" :: Void
      | hooks'
      )
      input
      slots
      output
      m
      ( "" :: Void
      )
      ( "" :: Void
      | hooks'
      )
      ( Hooks.HookHTML
          ( "" :: Void
          | hooks'
          )
          input
          slots
          output
          m
      )
  ) ->
  H.Component query input output m
component f =
  Hooks.component
    ( Hooks.defaultOptions
        { handleQuery =
          \q ->
            Hooks.getHooksM
              >>= \hooks -> case getHookCons (Proxy :: _ "") hooks of
                  Nothing -> Applicative.pure Nothing
                  Just fun -> (unsafeUnQ fun) q
        }
    )
    go
  where
  unsafeQ ::
    ( forall a.
      query a ->
      Hooks.HookM
        ( "" :: Void
        | hooks'
        )
        input
        slots
        output
        m
        (Maybe a)
    ) ->
    Void
  unsafeQ = unsafeCoerce

  unsafeUnQ ::
    Void ->
    ( forall a.
      query a ->
      Hooks.HookM
        ( "" :: Void
        | hooks'
        )
        input
        slots
        output
        m
        (Maybe a)
    )
  unsafeUnQ = unsafeCoerce

  start =
    Hooks.hookCons (Proxy :: _ "")
      (Applicative.pure ((unsafeQ (const $ Applicative.pure Nothing))))

  go =
    ( \i -> Ix.do
        _ <- start
        f
          { queryToken: QueryToken
          , slotsToken: SlotsToken
          , outputToken: OutputToken
          }
          i
    )

useQuery ::
  forall query hooks' input slots output m i.
  QueryToken query ->
  ( forall a.
    query a ->
    Hooks.HookM
      ( "" :: Void
      | hooks'
      )
      input
      slots
      output
      m
      (Maybe a)
  ) ->
  Hooks.IndexedHookM
    ( "" :: Void
    | hooks'
    )
    input
    slots
    output
    m
    i
    i
    Unit
useQuery _ fun = Hooks.lift (Hooks.setHookMCons (Proxy :: _ "") (unsafeQ fun))
  where
  unsafeQ ::
    ( forall a.
      query a ->
      Hooks.HookM
        ( "" :: Void
        | hooks'
        )
        input
        slots
        output
        m
        (Maybe a)
    ) ->
    Void
  unsafeQ = unsafeCoerce

useState ::
  forall hooks' hooks input slots output sym sym' m v i iRL o.
  RL.RowToList i iRL =>
  Sugar.GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Lacks sym hooks' =>
  Row.Cons sym v hooks' hooks =>
  v ->
  Hooks.IndexedHookM hooks input slots output m i o (v /\ Proxy sym)
useState = imap (flip (/\) (Proxy :: _ sym)) <<< Hooks.hookCons (Proxy :: _ sym) <<< Applicative.pure

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
