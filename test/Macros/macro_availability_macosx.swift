// REQUIRES: swift_swift_parser, OS=macosx

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name MacrosTest -target %target-cpu-apple-macosx11

@available(macOS 12.0, *)
struct X { }

@freestanding(expression) macro m1() -> X = #externalMacro(module: "A", type: "B") // expected-error{{'X' is only available in macOS 12.0 or newer}}
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm1()'}}

@available(macOS 12.0, *)
@freestanding(expression) macro m2() -> X = #externalMacro(module: "A", type: "B")
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm2()'}}
