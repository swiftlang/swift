// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip

// Make sure we don't crash
func testInvalidInInterpolation(_ x: Int) {
  _ = "\((x, \[]))" // expected-error {{invalid component of Swift key path}}
}
