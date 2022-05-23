{-
The `halogen-test-driver` contains a Halogen driver written for automated tests.
-}
let
  name = "halogen-test-driver"
in
  { name
  , dependencies =
      [ "halogen"
      , "halogen-vdom"
      , "pre"
      ]
  , packages = ../../packages.dhall
  -- Due to a spago bug (see https://github.com/purescript/spago/issues/648)
  -- `sources` are relative to root instead of config file.
  , sources = [ "lib/${name}/src/**/*.purs" ]
  }
