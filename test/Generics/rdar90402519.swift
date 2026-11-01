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
// expected-error@-1 {{same-type constraint 'Self.I' == 'G<Self.I>' is recursive}}
