{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "js-timers"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "tuples"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
