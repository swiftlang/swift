// REQUIRES: swift_swift_parser, executable_test, OS=macosx

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name MacrosTest -target %target-cpu-apple-macosx11  -load-plugin-library %t/%target-library-name(MacroDefinition)

@available(macOS 12.0, *)
struct X { }

@freestanding(expression) macro m1() -> X = #externalMacro(module: "A", type: "B") // expected-error{{'X' is only available in macOS 12.0 or newer}}
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm1()'}}

@available(macOS 12.0, *)
@freestanding(expression) macro m2() -> X = #externalMacro(module: "A", type: "B")
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm2()'}}

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@available(macOS 12.0, *)
func onlyInMacOS12() { }

func test() {
  _ = #stringify({
      if #available(macOS 12.0, *) {
        onlyInMacOS12()
      }
    })
}
