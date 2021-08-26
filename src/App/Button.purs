module App.Button where

import Prelude

import App.Hooks as Hooks
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Variant (inj)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State
  = { count :: Int }

data Action
  = Increment

affy0 :: Aff Int
affy0 = pure 0

component :: forall q i o. H.Component q i o Aff
component =
  Hooks.component Hooks.defaultOptions \_ -> Ix.do
    foo <- Hooks.hook (Proxy :: _ "foo") 0
    bar <- Hooks.hookAff (Proxy :: _ "bar") affy0
    ipure
      ( HH.div_
          [ HH.p_
              [ HH.text $ "Foo: " <> show foo <> " Bar: " <> show bar ]
          , HH.button
              [ HE.onClick \_ -> Hooks.modify (inj (Proxy :: _ "foo") (foo + 1)) ]
              [ HH.text "Incr foo" ]
          , HH.button
              [ HE.onClick \_ -> Hooks.modify (inj (Proxy :: _ "bar") (bar + 1)) ]
              [ HH.text "Incr bar" ]
          ]
      )
