// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest
// REQUIRES: OS=macosx

macro stringify<T>(_ value: T) -> (T, String) = BuiltinMacros.StringifyMacro

protocol P { }

macro tryToHide<T: P>(_: P) -> some P = BuiltinMacros.Blah
// expected-error@-1{{some' types are only permitted in properties, subscripts, and functions}}

internal struct X { } // expected-note{{type declared here}}

public macro createAnX: X = BuiltinMacros.Blah
// expected-error@-1{{macro cannot be declared public because its result type uses an internal type}}

func test(a: Int, b: Int) {
  // FIXME: Bad diagnostic.
  let s = #stringify<Int, Int>(a + b) // expected-error{{type of expression is ambiguous without more context}}
}

