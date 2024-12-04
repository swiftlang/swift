// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -disable-availability-checking

@attached(peer, names: named(BadThing))
macro HangingMacro() = #externalMacro(module: "MacroDefinition", type: "HangingMacro")

// expected-note@+1{{in declaration of 'Foo'}}
class Foo {
  init() {}

  // expected-note@+1 2{{in expansion of macro 'HangingMacro' on property 'result' here}}
  @HangingMacro var result: Int // This comment makes it hang.
}
