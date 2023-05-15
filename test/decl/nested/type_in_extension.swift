// RUN: %target-typecheck-verify-swift

struct G<T> {}

extension G {
  struct H<U> {
    func usesBoth<T1, U1>(t: T1, u: U1) -> (T1, U1) {}
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
