// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir
// REQUIRES: OS=macosx

macro stringify<T>(_ value: T) -> (T, String) = MacroDefinition.StringifyMacro
macro missingMacro1(_: Any) = MissingModule.MissingType // expected-note{{'missingMacro1' declared here}}
macro missingMacro2(_: Any) = MissingModule.MissingType

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

func testMissing() {
  #missingMacro1("hello") // expected-error{{external macro implementation type 'MissingModule.MissingType' could not be found for macro 'missingMacro1'; the type must be public and provided via '-load-plugin-library'}}
}
