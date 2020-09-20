{ name = "halogen-project"
, dependencies =
  [ "aff-bus"
  , "affjax"
  , "argonaut"
  , "bifunctors"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "psci-support"
  , "random"
  , "routing"
  , "routing-duplex"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
