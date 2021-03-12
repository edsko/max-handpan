{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "functions"
  , "integers"
  , "leibniz"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "undefined"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs", "purescript-maxforlive/src/**/*.purs" ]
}
