// RUN: %target-typecheck-verify-swift -enable-requirement-machine -debug-requirement-machine 2>&1 | %FileCheck %s

struct Foo<A, B> {}

protocol P1 {
  associatedtype X where X == Foo<Y1, Z1>
  associatedtype Y1
  associatedtype Z1
}

protocol P1a {
  associatedtype T : P1
}

protocol P2 {
  associatedtype X
  associatedtype Y2
  associatedtype Z2
}

protocol P2a {
  associatedtype T : P2 where T.X == Foo<T.Y2, T.Z2>
}

struct MergeTest<G : P1a & P2a> {
  func foo1(x: G.T.Y1) -> G.T.Y2 { return x }
  func foo2(x: G.T.Z1) -> G.T.Z2 { return x }
}

// CHECK-LABEL: Adding generic signature <τ_0_0 where τ_0_0 : P1a, τ_0_0 : P2a> {
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P2a:T] => τ_0_0.[P1a&P2a:T]
// CHECK: - τ_0_0.[P1a:T] => τ_0_0.[P1a&P2a:T]
// CHECK: - [P1a&P2a:T].[P2:X] => [P1a&P2a:T].[P1&P2:X]
// CHECK: - [P1a&P2a:T].[P1:X] => [P1a&P2a:T].[P1&P2:X]
// CHECK: - τ_0_0.[P1a&P2a:T].[P2:Y2] => τ_0_0.[P1a&P2a:T].[P1:Y1]
// CHECK: - τ_0_0.[P1a&P2a:T].[P2:Z2] => τ_0_0.[P1a&P2a:T].[P1:Z1]
// CHECK: }
// CHECK: }
