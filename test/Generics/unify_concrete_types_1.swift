// RUN: %target-typecheck-verify-swift -requirement-machine=verify -debug-requirement-machine 2>&1 | %FileCheck %s

struct Foo<A, B> {}

protocol P1 {
  associatedtype X where X == Foo<Y1, Z1>
  associatedtype Y1
  associatedtype Z1
}

protocol P2 {
  associatedtype X where X == Foo<Y2, Z2>
  associatedtype Y2
  associatedtype Z2
}

struct MergeTest<G : P1 & P2> {
  func foo1(x: G.Y1) -> G.Y2 { return x }
  func foo2(x: G.Z1) -> G.Z2 { return x }
}

// CHECK-LABEL: Adding generic signature <τ_0_0 where τ_0_0 : P1, τ_0_0 : P2> {
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P2:Y2] => τ_0_0.[P1:Y1]
// CHECK: - τ_0_0.[P2:Z2] => τ_0_0.[P1:Z1]
// CHECK: }
// CHECK-LABEL: Equivalence class map: {
// CHECK:  [P1:X] => { concrete_type: [concrete: Foo<τ_0_0, τ_0_1> with <[P1:Y1], [P1:Z1]>] }
// CHECK:  [P2:X] => { concrete_type: [concrete: Foo<τ_0_0, τ_0_1> with <[P2:Y2], [P2:Z2]>] }
// CHECK:  τ_0_0 => { conforms_to: [P1 P2] }
// CHECK:  τ_0_0.[P1&P2:X] => { concrete_type: [concrete: Foo<τ_0_0, τ_0_1> with <τ_0_0.[P2:Y2], τ_0_0.[P2:Z2]>] }
// CHECK: }
