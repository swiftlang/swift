// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype Value
}

protocol P2 {
  typealias A = Int
}

struct G<T: P1> where T.Value == any Collection, T.Value.Element: P2 {}
// expected-error@-1 {{cannot access associated type 'Element' from 'any Collection'; use a concrete type or generic parameter base instead}}

