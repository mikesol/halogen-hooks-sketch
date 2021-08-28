module App.Capture where

import Prelude

import App.Hooks as Hooks
import App.Sugar as Sugar
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
    foo <- Sugar.useState (Proxy :: _ "foo") 0
    Sugar.capture foo (lift $ Log.info "I changed!")
    bar <- Sugar.useState (Proxy :: _ "bar") 0
    ipure
      ( HH.div_
          [ HH.p_
              [ HH.text $ "Foo: " <> show foo <> " Bar: " <> show bar ]
          , HH.button
              [ HE.onClick \_ -> Sugar.set (Proxy :: _ "foo") (foo + 1) ]
              [ HH.text "Change foo" ]
          , HH.button
              [ HE.onClick \_ -> Sugar.set (Proxy :: _ "bar") (bar + 1) ]
              [ HH.text "Change bar" ]
          ]
      )
