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

  class M : S {}

  protocol P {
    associatedtype A
  }
}
