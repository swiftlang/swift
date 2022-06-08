// RUN: %target-typecheck-verify-swift -dump-requirement-machine 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T : P1
}

protocol P2 {
  associatedtype T where T == X<U>
  associatedtype U
}

extension Int : P1 {
  public typealias T = Int
}

struct X<A> : P1 {
  typealias T = X<A>
}

struct G<T : P1 & P2> {}

// Since G.T.T == G.T.T.T == G.T.T.T.T = ... = X<T.U>, we tie off the
// recursion by introducing a same-type requirement G.T.T => G.T.

// CHECK-LABEL: Requirement machine for fresh signature < T >
// CHECK-LABEL: Rewrite system: {
// CHECK: - [P1:T].T => [P1:T].[P1:T]
// CHECK: - τ_0_0.[P1:T].[concrete: X<τ_0_0.[P2:U]>] => τ_0_0.[P1:T]
// CHECK: - τ_0_0.[P1:T].[P1:T] => τ_0_0.[P1:T]
// CHECK: }
