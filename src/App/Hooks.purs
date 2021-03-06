module App.Hooks
  ( IndexedHookM
  , hookCons
  , component
  , getHooksM
  , lift
  , HookAction
  , HookM
  , HookArg
  , doThis
  , asHooks
  , setHookMCons
  , setHookMUnion
  , getHookCons
  , setHookCons
  , setHookUnion
  , Hooks
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class NotReadOnly
  , class NotReadOnlyRL
  , HookHTML
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad, iap)
import Data.Functor.Indexed (class IxFunctor)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Unsafe.Coerce (unsafeCoerce)

type HookM hooks input slots output m a
  = H.HalogenM
      { hooks :: (Hooks hooks)
      , input :: input
      , html :: HookHTML hooks input slots output m
      }
      (HookAction hooks input slots output m)
      slots
      output
      m
      a

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

data Hooks (r :: Row Type)

foreign import getHookConsFFI :: forall a r. Maybe a -> (a -> Maybe a) -> String -> Hooks r -> Maybe a

getHookCons ::
  forall proxy sym a r1 hooks.
  IsSymbol sym =>
  Cons sym a r1 hooks =>
  proxy sym ->
  Hooks hooks ->
  Maybe a
getHookCons = getHookConsFFI Nothing Just <<< reflectSymbol

foreign import setHookConsFFI :: forall a r. String -> a -> Hooks r -> Hooks r

setHookCons ::
  forall proxy sym a r1 hooks.
  IsSymbol sym =>
  Cons sym a r1 hooks =>
  proxy sym ->
  a ->
  Hooks hooks ->
  Hooks hooks
setHookCons = setHookConsFFI <<< reflectSymbol

data SetHookUnion

foreign import setHookUnionFFI :: forall r. SetHookUnion -> Hooks r -> Hooks r

setHookUnion ::
  forall r1 r2 hooks.
  Union r1 r2 hooks =>
  { | r1 } ->
  Hooks hooks ->
  Hooks hooks
setHookUnion = setHookUnionFFI <<< unsafeCoerce

setHookMCons ::
  forall proxy output input slots m sym a r1 hooks.
  NotReadOnly a =>
  Cons sym a r1 hooks =>
  IsSymbol sym =>
  proxy sym ->
  a ->
  HookM hooks input slots output m Unit
setHookMCons px a =
  ( H.modify_ \i ->
      i { hooks = setHookCons px a i.hooks }
  )

class NotReadOnlyRL (rl :: RL.RowList Type)

instance notReadOnlyRLNil :: NotReadOnlyRL RL.Nil

instance notReadOnlyRLCons :: (NotReadOnly b, NotReadOnlyRL c) => NotReadOnlyRL (RL.Cons a b c)

setHookMUnion ::
  forall output input slots m r1 rl r2 hooks.
  RL.RowToList r1 rl =>
  NotReadOnlyRL rl =>
  Union r1 r2 hooks =>
  { | r1 } ->
  HookM hooks input slots output m Unit
setHookMUnion r =
  ( H.modify_ \i ->
      i { hooks = setHookUnion r i.hooks }
  )

asHooks :: forall r. { | r } -> Hooks r
asHooks = unsafeCoerce

getHooksM ::
  forall hooks input slots output m.
  HookM hooks input slots output m (Hooks hooks)
getHooksM = H.gets _.hooks

class NotReadOnly (a :: Type)

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

type HookHTML hooks input slots output m
  = HC.HTML (H.ComponentSlot slots m (HookAction hooks input slots output m)) (HookAction hooks input slots output m)

type HookArg hooks input slots output m
  = input ->
    IndexedHookM hooks input slots output m () hooks (HookHTML hooks input slots output m)

doThis ::
  forall hooks input slots output m.
  HookM hooks input slots output m Unit ->
  HookAction hooks input slots output m
doThis = DoThis

handleHookAction ::
  forall hooks input slots output m rest.
  { finalize :: HookM hooks input slots output m Unit
  | rest
  } ->
  HookArg hooks input slots output m ->
  HookAction hooks input slots output m ->
  H.HalogenM
    { hooks :: (Hooks hooks)
    , input :: input
    , html :: HookHTML hooks input slots output m
    }
    (HookAction hooks input slots output m)
    slots
    output
    m
    Unit
handleHookAction { finalize } f = case _ of
  Initialize -> do
    { input } <- H.get
    html <- let IndexedHookM m = f input in m
    H.modify_ _ { html = html }
  DoThis m -> do
    m
    { input } <- H.get
    html <- let IndexedHookM m = f input in m
    H.modify_ _ { html = html }
  Receive i -> do
    { input } <- H.modify _ { input = i }
    html <- let IndexedHookM m = f input in m
    H.modify_ _ { html = html }
  Finalize -> finalize

type Options query hooks input slots output m
  = { receiveInput :: Boolean
    , handleQuery :: forall a. query a -> HookM hooks input slots output m (Maybe a)
    , finalize :: HookM hooks input slots output m Unit
    , initialHTML :: HookHTML hooks input slots output m
    }

defaultOptions ::
  forall query hooks input slots output m.
  Options query hooks input slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: \_ -> pure Nothing
  , finalize: pure unit
  , initialHTML: HH.div [] []
  }

component ::
  forall slots hooks query input output m.
  Options query hooks input slots output m ->
  HookArg hooks input slots output m ->
  H.Component query input output m
component options f =
  H.mkComponent
    { initialState: \input -> { input, hooks: unsafeCoerce {}, html: options.initialHTML }
    , render: \{ html } -> html
    , eval:
        H.mkEval
          { initialize: Just Initialize
          , finalize: Just Finalize
          , receive: if options.receiveInput then Just <<< Receive else const Nothing
          , handleAction: handleHookAction options f
          , handleQuery:
              \q -> let res = options.handleQuery q in res
          }
    }

newtype IndexedHookM (hooks :: Row Type) (input :: Type) (slots :: Row Type) (output :: Type) (m :: Type -> Type) (i :: Row Type) (o :: Row Type) a
  = IndexedHookM (HookM hooks input slots output m a)

derive instance indexedHookFFunctor :: Functor (IndexedHookM hooks input slots output m i i)

instance indexedHookFApply :: Apply (IndexedHookM hooks input slots output m i i) where
  apply = iapply

instance indexedHookFBind :: Bind (IndexedHookM hooks input slots output m i i) where
  bind = ibind

instance indexedHookFApplicative :: Applicative (IndexedHookM hooks input slots output m i i) where
  pure = ipure

instance indexedHookFMonad :: Monad (IndexedHookM hooks input slots output m i i)

instance indexedHookFIxFunctor :: IxFunctor (IndexedHookM hooks input slots output m) where
  imap f (IndexedHookM a) = IndexedHookM (map f a)

instance indexedHookFIxApply :: IxApply (IndexedHookM hooks input slots output m) where
  iapply = iap

instance indexedHookFIxApplicative :: IxApplicative (IndexedHookM hooks input slots output m) where
  ipure = IndexedHookM <<< pure

instance indexedHookFIxBind :: IxBind (IndexedHookM hooks input slots output m) where
  ibind (IndexedHookM fmonad) function = IndexedHookM (fmonad >>= \f -> let IndexedHookM o = function f in o)

instance indexedHookFIxMonad :: IxMonad (IndexedHookM hooks input slots output m)

data HookAction hooks input slots output m
  = Initialize
  | DoThis (HookM hooks input slots output m Unit)
  | Receive input
  | Finalize

lift ::
  forall hooks input slots output m v i.
  HookM hooks input slots output m v ->
  IndexedHookM hooks input slots output m i i v
lift = IndexedHookM

hookCons ::
  forall hooks' hooks input slots output proxy sym m v i o.
  IsSymbol sym =>
  Lacks sym i =>
  Cons sym v i o =>
  Lacks sym hooks' =>
  Cons sym v hooks' hooks =>
  proxy sym ->
  HookM hooks input slots output m v ->
  IndexedHookM hooks input slots output m i o v
hookCons px m = IndexedHookM go
  where
  go =
    getHookCons px <$> getHooksM
      >>= case _ of
          Nothing -> (m >>= setHookMCons px) *> go
          Just v -> pure v
