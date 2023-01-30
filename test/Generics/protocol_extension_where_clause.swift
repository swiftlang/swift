// RUN: %target-typecheck-verify-swift %s

protocol P<T> {
  associatedtype T
}

extension P {
  typealias A = Int
  // expected-note@-1 3{{consider moving 'A' into the definition of protocol 'P'}}
}

protocol Q {
  associatedtype T: P
  associatedtype U: P<T.A>
  // expected-error@-1 {{'A' was defined in extension of protocol 'P' and cannot be referenced from a associated type inheritance clause}}
}

struct S<T: P, U: P<T.A>> {
  // expected-error@-1 {{'A' was defined in extension of protocol 'P' and cannot be referenced from a generic parameter inheritance clause}}

  func f<V>(_: V) where V == T.A {}
  // expected-error@-1 {{'A' was defined in extension of protocol 'P' and cannot be referenced from a 'where' clause}}
}
