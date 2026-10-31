// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip

// Make sure we don't crash
func testInvalidInInterpolation(_ x: Int) {
  _ = "\((x, \[]))" // expected-cparser-error {{invalid component of Swift key path}} expected-astgen-error {{expected type in array type}}
}
