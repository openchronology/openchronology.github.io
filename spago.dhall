{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openchronology.github.io"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "prelude"
  , "promises"
  , "psci-support"
  , "quickcheck"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
