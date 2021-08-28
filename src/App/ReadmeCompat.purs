module App.RMButton where

import Prelude

import App.Hooks.Compat as Hooks
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = IsOn (Boolean -> a)

component :: forall i o. H.Component Query i o Aff
component = Hooks.component \{ queryToken } _ -> Hooks.do
  count /\ countId <- Hooks.useState 0
  Hooks.useQuery queryToken case _ of
    IsOn reply -> do
      pure (Just (reply true))
  Hooks.pure do
    HH.button
      [ HE.onClick \_ -> Hooks.modify_ countId (_ + 1) ]
      [ HH.text $ show count ]
