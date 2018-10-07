// RUN: %target-swift-frontend -parse -verify %s

// NOTE: The grammar of the '@autodiff' attribute is often ambiguous in a
// function type context. Things in parentheses can be interpreted as type
// names. To resolve this, the parser has to do fair amounts of backtracking,
// and parse failures in a '@autodiff(...)' often fall back to parsing the
// primary function type. This is why we don't add tests for diagnostics yet.

let _: @autodiff (T) -> U // okay
let _: @autodiff(blah) (T) -> U // okay
let _: @autodiff(order: 10) (T) -> U // okay
let _: @autodiff(forward, order: 10) (T) -> U // okay
let _: @autodiff (order) -> U // okay, parsed as a function type
let _: @autodiff (reverse, reverse) // okay, just a tuple
let _: @autodiff (reverse) -> U // okay, parsed as a function type
let _: @autodiff T // okay
