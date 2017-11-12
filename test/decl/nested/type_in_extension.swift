// RUN: %target-typecheck-verify-swift

struct G<T> {}

extension G {
  struct H<U> {
    func usesBoth<T, U>(t: T, u: U) -> (T, U) {}
  }
}

extension { // expected-error {{expected type name in extension declaration}}
  struct S<T> {
    func foo(t: T) {}
  }

  class M : S {} // expected-error {{use of undeclared type 'S'}}

  protocol P { // expected-error {{protocol 'P' cannot be nested inside another declaration}}
    associatedtype A
  }
}
