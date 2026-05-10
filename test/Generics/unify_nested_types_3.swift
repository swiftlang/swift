// RUN: %target-typecheck-verify-swift -dump-requirement-machine 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T : P1
}

protocol P2 {
  associatedtype T
}

struct X<A : P1> : P2 {
  typealias T = X<A.T>
}

protocol P2a {
  associatedtype T : P2
}

protocol P3a {
  associatedtype T where T == X<U>
  associatedtype U : P1
}

struct G<T : P2a & P3a> {}

// X.T has a DependentMemberType in it; make sure that we build the
// correct substitution schema.

// CHECK-LABEL: Requirement machine for fresh signature < T >
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P2a:T].[concrete: X<τ_0_0.[P3a:U]>] => τ_0_0.[P2a:T]
// CHECK: - τ_0_0.[P2a:T].[P2:T].[concrete: X<τ_0_0.[P3a:U].[P1:T]>] => τ_0_0.[P2a:T].[P2:T]
// CHECK: }
// CHECK-LABEL: Property map: {
// CHECK: τ_0_0.[P2a:T] => { conforms_to: [P2 Copyable Escapable] concrete_type: [concrete: X<τ_0_0.[P3a:U]>] }
// CHECK: τ_0_0.[P2a:T].[P2:T] => { conforms_to: [Copyable Escapable] concrete_type: [concrete: X<τ_0_0.[P3a:U].[P1:T]>] }
// CHECK: }
