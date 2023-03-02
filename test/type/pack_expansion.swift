// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func f1<each T>() -> repeat each T {}
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f2<each T>() -> (repeat each T) {}
// okay

struct G<each T> {}

func f3<each T>() -> G<repeat each T> {}

protocol P<T> {
  associatedtype T
}

func f4<each T>() -> any P<repeat each T> {}

typealias T1<each T> = repeat each T
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

typealias T2<each T> = (repeat each T)

func f4<each T>() -> repeat () -> each T {}
// expected-error@-1 {{variadic expansion '() -> T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f5<each T>() -> () -> (repeat each T) {}

func f6<each T>() -> (repeat each T) -> () {}

enum E<each T> {
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

func withWhereClause<each T>(_ x: repeat each T) where repeat each T: P {}
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

struct Outer<each T> {
  struct Bad<each U> {
    typealias Value = (repeat (each T, each U)) // expected-error {{variadic expansion 'repeat (each T, each U)' requires that 'T' and 'U' have the same shape}}
  }

  struct Good<each U> where (repeat (each T, each U)): Any {
    typealias Value = (repeat (each T, each U))
  }

  struct AlsoGood<each U> {
    typealias Value = (repeat (each T, E<repeat each U>))
  }
}

func packRef<each T>(_: repeat each T) where each T: P {}

func packMemberRef<each T>(_: repeat each T.T) where each T: P {}

// expected-error@+1 {{'each' cannot be applied to non-pack type 'Int'}}
func invalidPackRef(_: each Int) {}

// expected-error@+1 {{pack reference 'T' can only appear in pack expansion or generic requirement}}
func packRefOutsideExpansion<each T>(_: each T) {}
