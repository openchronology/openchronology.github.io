{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openchronology.github.io"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "debug"
  , "file-store"
  , "fixed-precision"
  , "node-fs-aff"
  , "promises"
  , "psci-support"
  , "react-mui"
  , "react-queue"
  , "spec"
  , "timeline"
  , "quickcheck-utf8"
  , "indexed-demiset"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "AGPL-3.0-or-later"
}
