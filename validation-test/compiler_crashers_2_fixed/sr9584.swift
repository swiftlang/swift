// RUN: %target-typecheck-verify-swift

struct S<N> {}

protocol P {
  associatedtype A: P = Self
  static func f(_ x: A) -> A
}

extension S: P where N: P {
  static func f<X: P>(_ x: X) -> S<X.A> where A == X, X.A == N {
  // expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
  // expected-note@-2 {{failed rewrite rule is τ_0_0.[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[concrete: S<S<S<S<S<S<S<S<S<S<S<S<S<S<τ_0_0>>>>>>>>>>>>>>] => τ_0_0.[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A].[P:A] [subst↓]}}
  // expected-error@-3 {{'A' is not a member type of type 'X'}}
  // expected-error@-4 {{'A' is not a member type of type 'X'}}
    return S<X.A>()
  }
}
