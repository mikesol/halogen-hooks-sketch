module App.OrderOfHooks where

import Prelude

import App.Hooks as Hooks
import App.Sugar as Sugar
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Halogen (lift)
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
    foo <- Sugar.hookConsPure (Proxy :: _ "foo") 0
    { bar, baz } <-
      if foo `mod` 2 == 0 then Ix.ado
        bar <- Sugar.hookConsPure (Proxy :: _ "bar") 0
        baz <- Sugar.hookConsPure (Proxy :: _ "baz") 0
        in { bar, baz }
      else Ix.ado
        baz <- Sugar.hookConsPure (Proxy :: _ "baz") 0
        bar <- Sugar.hookConsPure (Proxy :: _ "bar") 0
        in { bar, baz }
    ipure
      ( HH.div_
          [ HH.p_
              [ HH.text $ "Foo: " <> show foo <> " Bar: " <> show bar <> " Baz: " <> show baz ]
          , HH.button
              [ HE.onClick \_ -> Sugar.setM (Proxy :: _ "foo") (lift $ affAdd1 foo) ]
              [ HH.text "Incr foo" ]
          , HH.button
              [ HE.onClick \_ -> Sugar.set (Proxy :: _ "bar") (bar + 1) ]
              [ HH.text "Incr bar" ]
          , HH.button
              [ HE.onClick \_ -> Sugar.set (Proxy :: _ "baz") (baz + 1) ]
              [ HH.text "Incr baz" ]
          ]
      )
