{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "free"
  , "halogen"
  , "indexed-monad"
  , "maybe"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
