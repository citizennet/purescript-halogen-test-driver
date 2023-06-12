{-
{{GENERATED_DOC}}

The `halogen-test-driver` contains a Halogen driver written for automated tests.
-}
let
  name = "halogen-test-driver"
in
  { name
  , dependencies =
      [ "aff"
      , "arrays"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "foreign-object"
      , "fork"
      , "halogen"
      , "halogen-subscriptions"
      , "halogen-vdom"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "parallel"
      , "pre"
      , "refs"
      , "tailrec"
      , "tuples"
      ]
  -- This path is relative to config file
  , packages = {{PACKAGES_DIR}}/packages.dhall
  -- This path is relative to project root
  -- See https://github.com/purescript/spago/issues/648
  , sources = [ "{{SOURCES_DIR}}/src/**/*.purs" ]
  }
