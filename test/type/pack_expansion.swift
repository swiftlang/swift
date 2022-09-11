// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

func f1<@_typeSequence T>() -> T... {}
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f2<@_typeSequence T>() -> (T...) {}
// okay

struct G<@_typeSequence T> {}

func f3<@_typeSequence T>() -> G<T... > {}

protocol P<T> {
  associatedtype T
}

func f4<@_typeSequence T>() -> any P<T... > {}

typealias T1<@_typeSequence T> = T...
// expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

typealias T2<@_typeSequence T> = (T...)

func f4<@_typeSequence T>() -> () -> T... {}
// expected-error@-1 {{variadic expansion '() -> T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

func f5<@_typeSequence T>() -> () -> (T...) {}

func f6<@_typeSequence T>() -> (T...) -> () {}

enum E<@_typeSequence T> {
  case f1(_: T...)

  case f2(_: G<T... >)

  var x: T... { fatalError() }
  // expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

  var x: (T...) { fatalError() }

  subscript(_: T...) -> Int { fatalError() }

  subscript() -> T... { fatalError() }
  // expected-error@-1 {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}

  subscript() -> (T...) { fatalError() }
}