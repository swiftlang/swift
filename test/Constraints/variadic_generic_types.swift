// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

// Parsing an UnresolvedSpecializeExpr containing a PackExpansionType
struct G<T...> {}

func f<T...>(_: T...) {
  _ = G< >.self
  _ = G<Int>.self
  _ = G<Int, String>.self
  _ = G<T... >.self
  _ = G<Int, (Array<T>)... >.self
}

// Forming PackExpansionTypeReprs in simplifyTypeExpr()
func g<T...>(_: T...) {
  _ = (T...).self
  _ = (Int, T...).self
  _ = ((T...) -> ()).self
  _ = ((Int, (Array<T>)...) -> ()).self

  _ = (Int...).self // expected-error {{variadic expansion 'Int' must contain at least one variadic generic parameter}}
}

