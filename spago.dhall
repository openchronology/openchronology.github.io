{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openchronology.github.io"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "data-default"
  , "debug"
  , "file-store"
  , "fixed-precision"
  , "indexed-demiset"
  , "js-timers"
  , "node-fs-aff"
  , "promises"
  , "psci-support"
  , "quickcheck-utf8"
  , "react-markdown"
  , "react-mui"
  , "react-queue"
  , "spec"
  , "timeline"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "AGPL-3.0-or-later"
}
