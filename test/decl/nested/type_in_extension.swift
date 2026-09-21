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

  // A little unfortunate we emit this error, but because there's no
  // extended nominal, there's no way to do a qualified lookup.
  class M : S {} // expected-error {{cannot find type 'S' in scope}}

  protocol P {
    associatedtype A
  }
}
