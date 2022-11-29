// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros
protocol P { }
protocol Q { associatedtype Assoc }

macro m1: Int = A.M1
macro m2(_: Int) = A.M2
macro m3(a b: Int) -> Int = A.M3
macro m4<T: Q>: T = A.M4 where T.Assoc: P
macro m5<T: P>(_: T) = A.M4

macro m6 = A // expected-error{{expected '(' for macro parameters or ':' for a value-like macro}}
// expected-error@-1{{expected '.' between external macro module and type name}}
// expected-error@-2{{expected external macro type name}}
