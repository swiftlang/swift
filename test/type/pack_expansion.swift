// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func f1<T...>() -> repeat each T {}
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f2<T...>() -> (repeat each T) {}
// okay

struct G<T...> {}

func f3<T...>() -> G<repeat each T> {}

protocol P<T> {
  associatedtype T
}

func f4<T...>() -> any P<repeat each T> {}

typealias T1<T...> = repeat each T
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

typealias T2<T...> = (repeat each T)

func f4<T...>() -> repeat () -> each T {}
// expected-error@-1 {{variadic expansion '() -> T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f5<T...>() -> () -> (repeat each T) {}

func f6<T...>() -> (repeat each T) -> () {}

enum E<T...> {
  case f1(_: repeat each T)

  case f2(_: G<repeat each T>)

  var x: repeat each T { fatalError() }
  // expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

  var x: (repeat each T) { fatalError() }

  subscript(_: repeat each T) -> Int { fatalError() }

  subscript() -> repeat each T { fatalError() }
  // expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

  subscript() -> (repeat each T) { fatalError() }
}

func withWhereClause<T...>(_ x: repeat each T) where repeat each T: P {}
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

struct Outer<T...> {
  struct Bad<U...> {
    typealias Value = (repeat (each T, each U)) // expected-error {{variadic expansion '(T, U)...' requires that 'T' and 'U' have the same shape}}
  }

  struct Good<U...> where (repeat (each T, each U)): Any {
    typealias Value = (repeat (each T, each U))
  }

  struct AlsoGood<U...> {
    typealias Value = (repeat (T, E<repeat each U>))
  }
}

func packRef<T...>(_: repeat each T) where each T: P {}

func packMemberRef<T...>(_: repeat each T.T) where each T: P {}

// expected-error@+1 {{'each' cannot be applied to non-pack type 'Int'}}
func invalidPackRef(_: each Int) {}

// expected-error@+1 {{pack reference 'T' can only appear in pack expansion or generic requirement}}
func packRefOutsideExpansion<T...>(_: each T) {}
