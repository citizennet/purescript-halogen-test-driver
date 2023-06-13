# PureScript Halogen Test Driver

Modified version of [`Halogen.Aff.Driver.runUI`][runUI]. Instead of
logging a warning to the console when a duplicate slot is encountered, it will
emit a `duplicateSlot` event that can be captured by tests.

[runUI]: https://github.com/purescript-halogen/purescript-halogen/blob/23225596473898744a6cb84fde2791aa6fe8a66a/src/Halogen/Aff/Driver.purs#L108-L113
