// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

protocol P {}

protocol P1 {
  associatedtype A... // expected-error {{associated types cannot be variadic}}
  associatedtype B<U>...
  // expected-error@-1 {{associated types cannot be variadic}}
  // expected-error@-2 {{associated types must not have a generic parameter list}}
}

typealias Alias<T...> = (repeat each T)

struct S1<T...> {}
struct S2<T, U...> {}
struct S3<T..., U> {}

struct S4<T...:P, U> {}
struct S5<T... :P, U> {}
struct S6<T...: P> {}
struct S7<T... : P> {}

func foo<T...>(_ x: repeat each T) {}

func bar<T...:P>(_ x: repeat each T) {}
func baz<T... :P>(_ x: repeat each T) {}
func qux<T... : P>(_ x: repeat each T) {}
func quux<T...: P>(_ x: repeat each T) {}

func foobar<T, U, V...>(x: T, y: U, z: repeat each V) { }
func foobaz<T, U..., V>(x: T, y: repeat each U, z: V) { }
func fooqux<T..., U..., V...>(x: repeat each T, y: repeat each U, z: repeat each V) { }

// We allow whitespace between the generic parameter and the '...', this is
// consistent with regular variadic parameters.
func withWhitespace<T ...>(_ x: repeat each T) -> (repeat each T) {}
