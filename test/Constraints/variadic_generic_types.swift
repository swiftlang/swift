// REQUIRES: rdar104716322

// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

// Parsing an UnresolvedSpecializeExpr containing a PackExpansionType
struct G<T...> {}

func f<T...>(_: repeat each T) {
  _ = G< >.self
  _ = G<Int>.self
  _ = G<Int, String>.self
  _ = G<repeat each T>.self
  _ = G<Int, repeat Array<each T>>.self
}

// Forming PackExpansionTypeReprs in simplifyTypeExpr()
func g<T...>(_: repeat each T) {
  _ = (repeat each T).self
  _ = (Int, repeat each T).self
  _ = ((repeat each T) -> ()).self
  _ = ((Int, repeat Array<each T>) -> ()).self

  _ = (repeat each Int).self // expected-error {{variadic expansion 'Int' must contain at least one variadic generic parameter}}
}
