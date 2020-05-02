{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openchronology.github.io"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "console"
  , "debug"
  , "file-store"
  , "fixed-precision"
  , "promises"
  , "psci-support"
  , "quickcheck"
  , "react-mui"
  , "react-queue"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
