// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros
protocol P { }
protocol Q { associatedtype Assoc }

@expression macro m1: Int = #externalMacro(module: "A", type: "M1")
@expression macro m2(_: Int) = #externalMacro(module: "A", type: "M2")
@expression macro m3(a b: Int) -> Int = #externalMacro(module: "A", type: "M3")
@expression macro m4<T: Q>: T = #externalMacro(module: "A", type: "M4") where T.Assoc: P
@expression macro m5<T: P>(_: T) = #externalMacro(module: "A", type: "M4")

@expression macro m6 = A // expected-error{{expected '(' for macro parameters or ':' for a value-like macro}}
