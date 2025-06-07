// RUN: %target-typecheck-verify-swift -dump-requirement-machine 2>&1 | %FileCheck %s

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

// CHECK-LABEL: Requirement machine for fresh signature < G >
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P2a:T] => τ_0_0.[P1a:T]
// CHECK: - τ_0_0.[P1a:T].[P2] => τ_0_0.[P1a:T]
// CHECK: - τ_0_0.[P1a:T].[P2:X] => τ_0_0.[P1a:T].[P1:X]
// CHECK: }
// CHECK: Property map: {
// CHECK:   τ_0_0 => { conforms_to: [P1a P2a Copyable Escapable] }
// CHECK:   τ_0_0.[P1a:T] => { conforms_to: [P1 Copyable Escapable P2] }
// CHECK: }
