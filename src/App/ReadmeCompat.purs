module App.Button where

import Prelude

import App.Hooks.Compat as Hooks
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Tuple.Nested ((/\))

component :: forall q i o. H.Component q i o Aff
component = Hooks.component \_ input -> Hooks.do
  count /\ countId <- Hooks.useState 0

  Hooks.pure do
    HH.button
      [ HE.onClick \_ -> Hooks.modify_ countId (_ + 1) ]
      [ HH.text $ show count ]
