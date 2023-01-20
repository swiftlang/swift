// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest -target %target-cpu-apple-macosx11
// REQUIRES: OS=macosx

@available(macOS 12.0, *)
struct X { }

@expression macro m1: X = #externalMacro(module: "A", type: "B") // expected-error{{'X' is only available in macOS 12.0 or newer}}
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm1'}}

@available(macOS 12.0, *)
@expression macro m2: X = #externalMacro(module: "A", type: "B")
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm2'}}
