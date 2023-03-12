{ name = "js-blob"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "media-types"
  , "newtype"
  , "nullable"
  , "numbers"
  , "prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
