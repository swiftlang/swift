// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

protocol P {}

protocol P1 {
  associatedtype A... // expected-error {{associated types cannot be variadic}}
  associatedtype B<U>...
  // expected-error@-1 {{associated types cannot be variadic}}
  // expected-error@-2 {{associated types must not have a generic parameter list}}
}

typealias Alias<T...> = (T...)

struct S1<T...> {}
struct S2<T, U...> {}
struct S3<T..., U> {}

struct S4<T...:P, U> {}
struct S5<T... :P, U> {}
struct S6<T...: P> {}
struct S7<T... : P> {}

func foo<T...>(_ x: T...) {}

func bar<T...:P>(_ x: T...) {}
func baz<T... :P>(_ x: T...) {}
func qux<T... : P>(_ x: T...) {}
func quux<T...: P>(_ x: T...) {}

func foobar<T, U, V...>(x: T, y: U, z: V...) { }
func foobaz<T, U..., V>(x: T, y: U..., z: V) { }
func fooqux<T..., U..., V...>(x: T..., y: U..., z: V...) { }

// We allow whitespace between the generic parameter and the '...', this is
// consistent with regular variadic parameters.
func withWhitespace<T ...>(_ x: T ...) -> (T ...) {}
