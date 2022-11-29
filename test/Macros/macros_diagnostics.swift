// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest
// REQUIRES: OS=macosx

macro stringify<T>(_ value: T) -> (T, String) = _SwiftSyntaxMacros.StringifyMacro

protocol P { }

macro tryToHide<T: P>(_: P) -> some P = BuiltinMacros.Blah
// expected-error@-1{{some' types are only permitted in properties, subscripts, and functions}}

internal struct X { } // expected-note{{type declared here}}

public macro createAnX: X = BuiltinMacros.Blah
// expected-error@-1{{macro cannot be declared public because its result type uses an internal type}}

macro m1: Int = A.B
macro m1: Float = A.B

macro m2: Int = A.B // expected-note{{'m2' previously declared here}}
macro m2: Int = A.B // expected-error{{invalid redeclaration of 'm2'}}

macro m3(_: Int) -> Int = A.B
macro m3(_: Int) -> Float = A.B

macro m4(_: Int) -> Int = A.B // expected-note{{'m4' previously declared here}}
macro m4(_: Int) -> Int = A.B // expected-error{{invalid redeclaration of 'm4'}}

struct ZZZ {
  macro m5: Int = A.B
  // expected-error@-1{{macro 'm5' can only be declared at file scope}}
}

func test(a: Int, b: Int) {
  // FIXME: Bad diagnostic.
  let s = #stringify<Int, Int>(a + b) // expected-error{{type of expression is ambiguous without more context}}
}

func shadow(a: Int, b: Int, stringify: Int) {
  _ = #stringify(a + b)
}
