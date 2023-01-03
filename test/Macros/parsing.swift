// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros
protocol P { }
protocol Q { associatedtype Assoc }

@expression macro m1: Int = A.M1
@expression macro m2(_: Int) = A.M2
@expression macro m3(a b: Int) -> Int = A.M3
@expression macro m4<T: Q>: T = A.M4 where T.Assoc: P
@expression macro m5<T: P>(_: T) = A.M4

@expression macro m6 = A // expected-error{{expected '(' for macro parameters or ':' for a value-like macro}}
// expected-error@-1{{expected '.' between external macro module and type name}}
// expected-error@-2{{expected external macro type name}}
