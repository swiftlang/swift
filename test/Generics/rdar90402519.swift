// RUN: %target-typecheck-verify-swift

// This needs a better diagnostic. The real problem is the 'C == G<I>'
// requirement in P2 conflicts with the one in P1.

protocol P {
  associatedtype I
}

struct G1<I> : P {}
struct G2<T> : P {
  typealias I = G<T>
}

struct G<T> {}

protocol P0 {
  associatedtype C : P
}

protocol P1 : P0 where C == G1<I> {
  associatedtype I
}

protocol P2 : P1 where C == G2<I> {}
// expected-error@-1 {{cannot build rewrite system for protocol; concrete type difference limit exceeded}}
// expected-note@-2 {{failed rewrite rule is [P2:I].[concrete: G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<G<[P2:I]>>>>>>>>>>>>>>>>>>>>>>>>>> : Escapable] => [P2:I]}}
