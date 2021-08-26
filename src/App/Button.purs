module App.Button where

import Prelude
import App.Hooks as Hooks
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State
  = { count :: Int }

data Action
  = Increment

aff0 :: Aff Int
aff0 = pure 0

affAdd1 :: Int -> Aff Int
affAdd1 = pure <<< add 1

component :: forall q i o. H.Component q i o Aff
component =
  Hooks.component Hooks.defaultOptions \_ -> Ix.do
    foo <- Hooks.hook (Proxy :: _ "foo") 0
    bar <- Hooks.hookM (Proxy :: _ "bar") aff0
    ipure
      ( HH.div_
          [ HH.p_
              [ HH.text $ "Foo: " <> show foo <> " Bar: " <> show bar ]
          , HH.button
              [ HE.onClick \_ -> Hooks.modifyM (Proxy :: _ "foo") (affAdd1 foo) ]
              [ HH.text "Incr foo" ]
          , HH.button
              [ HE.onClick \_ -> Hooks.modify (Proxy :: _ "bar") (bar + 1) ]
              [ HH.text "Incr bar" ]
          ]
      )
