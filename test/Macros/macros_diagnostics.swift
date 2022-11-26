// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest
// REQUIRES: OS=macosx

macro stringify<T>(_ value: T) -> (T, String) = BuiltinMacros.StringifyMacro

func test(a: Int, b: Int) {
  // FIXME: Bad diagnostic.
  let s = #stringify<Int, Int>(a + b) // expected-error{{type of expression is ambiguous without more context}}
}
