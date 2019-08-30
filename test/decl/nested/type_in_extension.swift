// RUN: %target-typecheck-verify-swift

struct G<T> {}

extension G {
  struct H<U> {
    func usesBoth<T, U>(t: T, u: U) -> (T, U) {}
  }
}

extension G {
  struct S<T> { // expected-note {{generic type 'S' declared here}}
    func foo(t: T) {}
  }

  class M : S {} // expected-error {{reference to generic type 'G<T>.S' requires arguments in <...>}}

  protocol P { // expected-error {{protocol 'P' cannot be nested inside another declaration}}
    associatedtype A
  }
}
