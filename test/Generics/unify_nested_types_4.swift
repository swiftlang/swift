// RUN: %target-typecheck-verify-swift -dump-requirement-machine 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A : P1
  associatedtype B : P1
}

struct S1 : P1 {
  typealias A = S1
  typealias B = S2
}

struct S2 : P1 {
  typealias A = S2
  typealias B = S1
}

protocol P2 {
  associatedtype A where A == S1
  associatedtype B where B == S2
}

struct G<T : P1 & P2> {}

// T.A and T.B become concrete, which produces the following series of
// concretized nested types:
//
// T.A.[concrete: S1]
// T.B.[concrete: S2]
// T.A.A.[concrete: S1]
// T.A.B.[concrete: S2]
// T.B.A.[concrete: S2]
// T.B.B.[concrete: S1]
// ...
//
// This would normally go on forever, but since S1 and S2 are not generic,
// we solve this by merging the repeated types with T.A or T.B:
//
// T.A.A => T.A
// T.B.A => T.B
// ...

// CHECK-LABEL: Requirement machine for fresh signature < T >
// CHECK-LABEL: Rewrite system: {
// CHECK: - τ_0_0.[P1:A].[concrete: S1 : P1] => τ_0_0.[P1:A]
// CHECK: - τ_0_0.[P1:A].[P1:A] => τ_0_0.[P1:A]
// CHECK: - τ_0_0.[P1:A].[P1:B].[concrete: S2] => τ_0_0.[P1:A].[P1:B]
// CHECK: - τ_0_0.[P1:B].[concrete: S2 : P1] => τ_0_0.[P1:B]
// CHECK: - τ_0_0.[P1:B].[P1:A] => τ_0_0.[P1:B]
// CHECK: - τ_0_0.[P1:B].[P1:B].[concrete: S1] => τ_0_0.[P1:B].[P1:B]
// CHECK: - τ_0_0.[P1:A].[P1:B].[concrete: S2 : P1] => τ_0_0.[P1:A].[P1:B]
// CHECK: - τ_0_0.[P1:A].[P1:B].[P1:A] => τ_0_0.[P1:A].[P1:B]
// CHECK: - τ_0_0.[P1:A].[P1:B].[P1:B] => τ_0_0.[P1:A]
// CHECK: - τ_0_0.[P1:B].[P1:B].[concrete: S1 : P1] => τ_0_0.[P1:B].[P1:B]
// CHECK: - τ_0_0.[P1:B].[P1:B].[P1:A] => τ_0_0.[P1:B].[P1:B]
// CHECK: - τ_0_0.[P1:B].[P1:B].[P1:B] => τ_0_0.[P1:B]
// CHECK: }
// CHECK-LABEL: Property map: {
// CHECK: τ_0_0.[P1:A] => { conforms_to: [P1 Copyable Escapable] concrete_type: [concrete: S1] }
// CHECK: τ_0_0.[P1:B] => { conforms_to: [P1 Copyable Escapable] concrete_type: [concrete: S2] }
// CHECK: τ_0_0.[P1:A].[P1:B] => { conforms_to: [P1 Copyable Escapable] concrete_type: [concrete: S2] }
// CHECK: τ_0_0.[P1:B].[P1:B] => { conforms_to: [P1 Copyable Escapable] concrete_type: [concrete: S1] }
// CHECK: }
