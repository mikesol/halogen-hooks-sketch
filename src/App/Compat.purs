module App.Hooks.Compat
  ( useState
  , useQuery
  , useTickEffect
  , useLifecycleEffect
  , component
  , modify_
  , capture
  , QueryToken
  , SlotsToken
  , OutputToken
  , Q
  , F
  , pure
  , bind
  , discard
  ) where

import Prelude
import App.Hooks as Hooks
import App.Sugar as Sugar
import Control.Applicative.Indexed (class IxApplicative, ipure, (:*>))
import Control.Bind.Indexed (class IxBind, class IxDiscard, ibind, idiscard, imap)
import Control.Monad.Indexed.Qualified as Ix
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Prelude as Applicative
import Prelude as Bind
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))

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

data Q query hooks' input slots output m
  = Q
    ( forall a.
      query a ->
      Hooks.HookM
        ( "" :: Q query hooks' input slots output m
        , "_" :: F query hooks' input slots output m
        | hooks'
        )
        input
        slots
        output
        m
        (Maybe a)
    )

unQ ::
  forall query hooks' input slots output m.
  Q query hooks' input slots output m ->
  ( forall a.
    query a ->
    Hooks.HookM
      ( "" :: Q query hooks' input slots output m
      , "_" :: F query hooks' input slots output m
      | hooks'
      )
      input
      slots
      output
      m
      (Maybe a)
  )
unQ (Q q) = q

data F query hooks' input slots output m
  = F
    ( Array
        ( Hooks.HookM
            ( "" :: Q query hooks' input slots output m
            , "_" :: F query hooks' input slots output m
            | hooks'
            )
            input
            slots
            output
            m
            Unit
        )
    )

unF ::
  forall query hooks' input slots output m.
  F query hooks' input slots output m ->
  Array
    ( Hooks.HookM
        ( "" :: Q query hooks' input slots output m
        , "_" :: F query hooks' input slots output m
        | hooks'
        )
        input
        slots
        output
        m
        Unit
    )
unF (F q) = q

component ::
  forall (query :: Type -> Type) (hooks' :: Row Type) (input :: Type) (slots :: Row Type) (output :: Type) (m :: Type -> Type).
  Row.Lacks "" hooks' =>
  Row.Lacks "_" hooks' =>
  ( ComponentTokens query slots output ->
    input ->
    Hooks.IndexedHookM
      ( "" :: Q query hooks' input slots output m
      , "_" :: F query hooks' input slots output m
      | hooks'
      )
      input
      slots
      output
      m
      ( "" :: Q query hooks' input slots output m
      , "_" :: F query hooks' input slots output m
      )
      ( "" :: Q query hooks' input slots output m
      , "_" :: F query hooks' input slots output m
      | hooks'
      )
      ( Hooks.HookHTML
          ( "" :: Q query hooks' input slots output m
          , "_" :: F query hooks' input slots output m
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
              >>= \hooks -> case Hooks.getHookCons (Proxy :: _ "") hooks of
                  Nothing -> Applicative.pure Nothing
                  Just fun -> (unQ fun) q
        , finalize =
          Hooks.getHooksM
            >>= \hooks -> case Hooks.getHookCons (Proxy :: _ "_") hooks of
                Nothing -> Applicative.pure unit
                Just arr -> void $ sequence (unF arr)
        }
    )
    go
  where
  start =
    Hooks.hookCons (Proxy :: _ "")
      (Applicative.pure (Q (const $ Applicative.pure Nothing)))
      :*> Hooks.hookCons (Proxy :: _ "_")
          (Applicative.pure (F []))

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

useLifecycleEffect ::
  forall i o hooks'' input slots output m query hooks' iRL sym' sym.
  IsSymbol sym =>
  RL.RowToList i iRL =>
  Sugar.GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  Row.Lacks sym i =>
  Row.Cons sym Unit i o =>
  Row.Lacks sym hooks'' =>
  Row.Cons
    sym
    Unit
    hooks''
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    ) =>
  Hooks.HookM
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    )
    input
    slots
    output
    m
    ( Maybe
        ( Hooks.HookM
            ( "" :: Q query hooks' input slots output m
            , "_" :: F query hooks' input slots output m
            | hooks'
            )
            input
            slots
            output
            m
            Unit
        )
    ) ->
  Hooks.IndexedHookM
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    )
    input
    slots
    output
    m
    i
    o
    Unit
useLifecycleEffect m = Hooks.hookCons (Proxy :: _ sym) (Applicative.pure unit) :*> useTickEffect m

useTickEffect ::
  forall query hooks' input slots output m i.
  Hooks.HookM
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    )
    input
    slots
    output
    m
    ( Maybe
        ( Hooks.HookM
            ( "" :: Q query hooks' input slots output m
            , "_" :: F query hooks' input slots output m
            | hooks'
            )
            input
            slots
            output
            m
            Unit
        )
    ) ->
  Hooks.IndexedHookM
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    )
    input
    slots
    output
    m
    i
    i
    Unit
useTickEffect i =
  Hooks.lift
    ( Bind.bind i \iRes ->
        Bind.bind
          ( map
              ( fromMaybe (F [])
                  <<< Hooks.getHookCons (Proxy :: _ "_")
              )
              Hooks.getHooksM
          ) \(F arr) ->
          void
            $ Hooks.setHookMCons (Proxy :: _ "_")
            $ F
            $ case iRes of
                Nothing -> arr
                Just iRes' -> [ iRes' ] <> arr
    )

useQuery ::
  forall query hooks' input slots output m i.
  QueryToken query ->
  ( forall a.
    query a ->
    Hooks.HookM
      ( "" :: Q query hooks' input slots output m
      , "_" :: F query hooks' input slots output m
      | hooks'
      )
      input
      slots
      output
      m
      (Maybe a)
  ) ->
  Hooks.IndexedHookM
    ( "" :: Q query hooks' input slots output m
    , "_" :: F query hooks' input slots output m
    | hooks'
    )
    input
    slots
    output
    m
    i
    i
    Unit
useQuery _ fun = Hooks.lift (Hooks.setHookMCons (Proxy :: _ "") (Q fun))

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

capture ::
  forall iRL sym' hooks' hooks input slots output m sym v i o.
  RL.RowToList i iRL =>
  Sugar.GetLexicalLast "" iRL sym' =>
  Symbol.Append sym' "_" sym =>
  IsSymbol sym =>
  Row.Lacks sym i =>
  Row.Cons sym v i o =>
  Row.Lacks sym hooks' =>
  Row.Cons sym v hooks' hooks =>
  Eq v =>
  v ->
  Hooks.HookM hooks input slots output m Unit ->
  Hooks.IndexedHookM hooks input slots output m i o Unit
capture = Sugar.capture
