// RUN: %target-typecheck-verify-swift

class G<T> {}

protocol P1 { // expected-error {{cannot build rewrite system for protocol; concrete type difference limit exceeded}}
// expected-note@-1 {{failed rewrite rule is [P1:B].[superclass: G<G<G<G<G<G<G<G<G<G<G<G<G<[P1].A>>>>>>>>>>>>>] => [P1:B]}}
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
