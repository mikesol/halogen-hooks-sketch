module App.Capture where

import Prelude
import App.Hooks as Hooks
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Effect.Class.Console as Log
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State
  = { count :: Int }

data Action
  = Increment

component :: forall q i o. H.Component q i o Aff
component =
  Hooks.component Hooks.defaultOptions \_ -> Ix.do
    foo <- Hooks.hook (Proxy :: _ "foo") 0
    Hooks.capture foo (lift $ Log.info "I changed!")
    bar <- Hooks.hook (Proxy :: _ "bar") 0
    ipure
      ( HH.div_
          [ HH.p_
              [ HH.text $ "Foo: " <> show foo <> " Bar: " <> show bar ]
          , HH.button
              [ HE.onClick \_ -> Hooks.set (Proxy :: _ "foo") (foo + 1) ]
              [ HH.text "Change foo" ]
          , HH.button
              [ HE.onClick \_ -> Hooks.set (Proxy :: _ "bar") (bar + 1) ]
              [ HH.text "Change bar" ]
          ]
      )
