{ name = "purescript-d3-bindings"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "halogen"
  , "halogen-storybook"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "web-dom"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
