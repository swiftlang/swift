// RUN: %target-typecheck-verify-swift

public protocol P1 {
  associatedtype A: P1 where A.A == A
}

public protocol P2 {
  associatedtype B: P1
  associatedtype C: P2
}

public struct G<B : P1> : P2 {
  public typealias C = G<B.A>
}

// C == G<B> together with the definition of G<B>.C implies an infinite series
// of rules:
// - C.C == G<B.A>
// - C.C.C == G<B.A.A>
// - C.C.C.C == G<B.A.A.A>
// - ...
//
// This would normally prevent the completion procedure from terminating,
// however A.A == A in protocol P1 means that the right hand sides simplify
// as follows:
//
// - C.C == G<B.A>
// - C.C.C == B<G.A>
// - C.C.C.C == G<B.A>
// - ...
//
// Therefore, the single rule C.C == C.C.C suffices to "tie off" the recursion.
public protocol P3: P2 where C == G<B> {}
