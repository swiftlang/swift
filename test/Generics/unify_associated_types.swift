// RUN: %target-typecheck-verify-swift -requirement-machine=verify -debug-requirement-machine 2>&1 | %FileCheck %s

struct Foo<A, B> {}

protocol P1 {
  associatedtype X
}

protocol P1a {
  associatedtype T : P1
}

protocol P2 {
  associatedtype X
}

protocol P2a {
  associatedtype T : P2
}

struct MergeTest<G : P1a & P2a> {}

// CHECK-LABEL: Adding generic signature <τ_0_0 where τ_0_0 : P1a, τ_0_0 : P2a> {
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P2a:T] => τ_0_0.[P1a&P2a:T]
// CHECK: - τ_0_0.[P1a:T] => τ_0_0.[P1a&P2a:T]
// CHECK: - τ_0_0.[P1a&P2a:T].[P1:X] => τ_0_0.[P1a&P2a:T].[P1&P2:X]
// CHECK: - [P1a&P2a:T].[P2:X] => [P1a&P2a:T].[P1&P2:X]
// CHECK: - [P1a&P2a:T].[P1:X] => [P1a&P2a:T].[P1&P2:X]
// CHECK: }
// CHECK: Property map: {
// CHECK:   [P1a&P2a:T] => { conforms_to: [P1 P2] }
// CHECK:   [P1a:T] => { conforms_to: [P1] }
// CHECK:   [P2a:T] => { conforms_to: [P2] }
// CHECK:   τ_0_0 => { conforms_to: [P1a P2a] }
// CHECK: }
// CHECK: }

