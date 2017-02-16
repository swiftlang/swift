// RUN: %target-typecheck-verify-swift -typecheck %s
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

protocol P {
    associatedtype A
    typealias X = A
}
protocol Q {
    associatedtype B: P
}

// CHECK-LABEL: .requirementOnNestedTypeAlias@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Q [Explicit @ 19:51]
// CHECK-NEXT:   τ_0_0[.Q].B : P [Explicit @ 19:51 -> Protocol requirement (Q)]
// CHECK-NEXT:   τ_0_0[.Q].B[.P].A == Int [Explicit @ 19:62]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q, τ_0_0.B.A == Int>
func requirementOnNestedTypeAlias<T>(_: T) where T: Q, T.B.X == Int {}

struct S<T> {}

protocol P2 {
    associatedtype A
    typealias X = S<A>
}
protocol Q2 {
    associatedtype B: P2
    associatedtype C
}

// CHECK-LABEL: .requirementOnConcreteNestedTypeAlias@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Q2 [Explicit @ 39:59]
// CHECK-NEXT:   τ_0_0[.Q2].B : P2 [Explicit @ 39:59 -> Protocol requirement (Q2)]
// CHECK-NEXT:   τ_0_0[.Q2].C == S<T.B.A> [Explicit @ 39:69]
// CHECK-NEXT:   τ_0_0[.Q2].B[.P2].X == S<T.B.A> [Nested type match]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.C == S<τ_0_0.B.A>>
func requirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, T.C == T.B.X {}

// CHECK-LABEL: .concreteRequirementOnConcreteNestedTypeAlias@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Q2 [Explicit @ 48:67]
// CHECK-NEXT:   τ_0_0[.Q2].B : P2 [Explicit @ 48:67 -> Protocol requirement (Q2)]
// CHECK-NEXT:   τ_0_0[.Q2].C == τ_0_0[.Q2].B[.P2].A [Explicit]
// CHECK-NEXT:   τ_0_0[.Q2].B[.P2].X == S<T.B.A> [Nested type match]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.C == τ_0_0.B.A>
func concreteRequirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, S<T.C> == T.B.X {}
