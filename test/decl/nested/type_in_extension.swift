// RUN: %target-parse-verify-swift

struct G<T> {}

extension G {
  struct H<U> { } // expected-error {{type 'H' cannot be nested in extension of generic type 'G'}}
}

extension { // expected-error {{expected type name in extension declaration}}
  struct S<T> {
    func foo(t: T) {} // expected-error {{use of undeclared type 'T'}}
  }

  class M : S {} // expected-error {{use of undeclared type 'S'}}

  protocol P { // expected-error {{protocol 'P' cannot be nested inside another declaration}}
    associatedtype A
  }
}
