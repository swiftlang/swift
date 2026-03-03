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

public protocol K3: ~Copyable {
  associatedtype A
}

// This shouldn't work because the extension defining B is implicitly constrained to K4's where Self: Copyable
public protocol K4: K3
  where A == B, // expected-error {{type 'Self' does not conform to protocol 'Copyable'}}
        Self: ~Copyable {}

extension K4 {
  public typealias B = String
}
