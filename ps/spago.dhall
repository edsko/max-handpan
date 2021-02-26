{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "functions"
  , "integers"
  , "leibniz"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "psci-support"
  , "refs"
  , "undefined"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs", "purescript-maxforlive/src/**/*.purs" ]
}
