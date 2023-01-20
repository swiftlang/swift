// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

// Parsing an UnresolvedSpecializeExpr containing a PackExpansionType
struct G<T...> {}

func f<T...>(_: repeat each T) {
  _ = G< >.self
  _ = G<Int>.self
  _ = G<Int, String>.self
  _ = G<repeat T>.self
  _ = G<Int, repeat Array<T>>.self
}

// Forming PackExpansionTypeReprs in simplifyTypeExpr()
func g<T...>(_: repeat each T) {
  _ = (repeat T).self
  _ = (Int, repeat T).self
  _ = ((repeat T) -> ()).self
  _ = ((Int, repeat Array<T>) -> ()).self

  _ = (repeat Int).self // expected-error {{variadic expansion 'Int' must contain at least one variadic generic parameter}}
}

