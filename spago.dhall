{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openchronology.github.io"
, dependencies =
  [ "debug"
  , "file-store"
  , "fixed-precision"
  , "js-timers"
  , "node-fs-aff"
  , "parseint"
  , "promises"
  , "psci-support"
  , "quickcheck-utf8"
  , "react-markdown"
  , "react-mui"
  , "react-queue"
  , "spec"
  , "stringutils"
  , "timeline"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "AGPL-3.0-or-later"
}
