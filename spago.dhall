{ name = "hyrule"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "contravariant"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "filterable"
  , "random"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "refs"
  , "safe-coerce"
  , "st"
  , "tailrec"
  , "these"
  , "tuples"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, license = "BSD-3-Clause"
, repository = "https://github.com/mikesol/purescript-hyrule"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
