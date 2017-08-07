// RUN: %target-swift-frontend -enforce-exclusivity=checked -Onone -emit-sil -swift-version 4 -verify -parse-as-library %s
//
// This is an adjunct to access_enforcement_noescape.swift to cover early static diagnostics.

// Helper
func doOneInout(_: ()->(), _: inout Int) {}

// Error: Cannot capture nonescaping closure.
// expected-note@+1{{parameter 'fn' is implicitly non-escaping}}
func reentrantCapturedNoescape(fn: (() -> ()) -> ()) {
  // expected-error@+1{{closure use of non-escaping parameter 'fn' may allow it to escape}}
  let c = { fn {} }
  fn(c)
}

// Error: inout cannot be captured.
func inoutReadBoxWriteInout(x: inout Int) {
  // expected-error@+1{{escaping closures can only capture inout parameters explicitly by value}}
  let c = { _ = x }
  doOneInout(c, &x)
}

// Error: Cannot capture inout
func inoutWriteBoxWriteInout(x: inout Int) {
  // expected-error@+1{{escaping closures can only capture inout parameters explicitly by value}}
  let c = { x = 42 }
  doOneInout(c, &x)
}
