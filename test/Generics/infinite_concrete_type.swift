// RUN: %target-typecheck-verify-swift

class G<T> {}

protocol P1 { // expected-error {{cannot build rewrite system for protocol; concrete type difference limit exceeded}}
// expected-note@-1 {{failed rewrite rule is [P1:B].[superclass: G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<[P1].B>>>>>>>>>>>>>>>>>>>>>>] => [P1:B]}}
  associatedtype A where A == G<B>
  associatedtype B where B == G<A>
}

// The GenericSignatureBuilder rejected this protocol, but there's no real
// reason to do that.
protocol P2 {
  associatedtype A where A : G<B>
  associatedtype B where B : G<A>
}

func useP2<T : P2>(_: T) {
  _ = T.A.self
  _ = T.B.self
}

protocol P3 {
  associatedtype T : P3
}

struct S<U : P3> : P3 {
  typealias T = S<S<U>>
}

protocol P4Base {
  associatedtype T : P3
  associatedtype U : P3
}

protocol P4 : P4Base where T == S<U> {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is [P4:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[concrete: S<S<S<S<S<S<S<S<S<S<S<S<S<S<[P4:U]>>>>>>>>>>>>>>] => [P4:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T].[P3:T]}}
}

protocol Exponential {
// expected-error@-1 {{cannot build rewrite system for protocol; concrete type size limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A where A == (A, A)
}

class Base<T> {}

class Derived<T, U> : Base<(T, U)> {}

protocol TooManyDifferences {
// expected-error@-1 {{cannot build rewrite system for protocol; concrete type difference limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A1 where A1: Derived<B, C>, A2: Base<B>, A1 == A2
  associatedtype A2
  associatedtype B
  associatedtype C
}