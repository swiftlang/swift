// RUN: %swift %s -enable-definite-init -verify

// These are tests for definite initialization, which is implemented by the
// memory promotion pass.

func foo() -> Int {
  var a : Int  // expected-note {{variable defined here}}
  return a     // expected-error {{variable used before being initialized}}
}
