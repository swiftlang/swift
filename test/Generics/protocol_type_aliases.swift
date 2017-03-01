// RUN: %target-typecheck-verify-swift -typecheck %s
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump


func sameType<T>(_: T.Type, _: T.Type) {}

protocol P {
    associatedtype A
    typealias X = A
}
protocol Q {
    associatedtype B: P
}

// CHECK-LABEL: .requirementOnNestedTypeAlias@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Q [τ_0_0: Explicit @ 22:51]
// CHECK-NEXT:   τ_0_0[.Q].B : P [τ_0_0: Explicit @ 22:51 -> Protocol requirement (Q)]
// CHECK-NEXT:   τ_0_0[.Q].B[.P].A == Int [τ_0_0[.Q].B[.P].X: Explicit @ 22:62]
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
// CHECK-NEXT:   τ_0_0 : Q2 [τ_0_0: Explicit @ 42:59]
// CHECK-NEXT:   τ_0_0[.Q2].B : P2 [τ_0_0: Explicit @ 42:59 -> Protocol requirement (Q2)]
// CHECK-NEXT:   τ_0_0[.Q2].C == S<T.B.A> [τ_0_0[.Q2].C: Explicit @ 42:69]
// CHECK-NEXT:   τ_0_0[.Q2].B[.P2].X == S<T.B.A> [τ_0_0[.Q2].B[.P2].X: Nested type match]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.C == S<τ_0_0.B.A>>
func requirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, T.C == T.B.X {}

// CHECK-LABEL: .concreteRequirementOnConcreteNestedTypeAlias@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Q2 [τ_0_0: Explicit @ 51:67]
// CHECK-NEXT:   τ_0_0[.Q2].B : P2 [τ_0_0: Explicit @ 51:67 -> Protocol requirement (Q2)]
// CHECK-NEXT:   τ_0_0[.Q2].C == τ_0_0[.Q2].B[.P2].A [τ_0_0[.Q2].C: Explicit]
// CHECK-NEXT:   τ_0_0[.Q2].B[.P2].X == S<T.B.A> [τ_0_0[.Q2].B[.P2].X: Nested type match]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.C == τ_0_0.B.A>
func concreteRequirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, S<T.C> == T.B.X {}


// Incompatible concrete typealias types are flagged as such
protocol P3 {
    typealias T = Int // expected-error{{typealias 'T' requires types 'Int' and 'Float' to be the same}}
}
protocol Q3: P3 {
    typealias T = Float
}

protocol P3_1 {
    typealias T = Float // expected-error{{typealias 'T' requires types 'Float' and 'Int' to be the same}}
}
protocol Q3_1: P3, P3_1 {}

// FIXME: these shouldn't be necessary to trigger the errors above, but are, due to
// the 'recusive decl validation' FIXME in GenericSignatureBuilder.cpp.
func useTypealias<T: Q3>(_: T, _: T.T) {}
func useTypealias1<T: Q3_1>(_: T, _: T.T) {}

// Subprotocols can force associated types in their parents to be concrete, and
// this should be understood for types constrained by the subprotocols.
protocol Q4: P {
    typealias A = Int
}
protocol Q5: P {
    typealias X = Int
}

// fully generic functions that manipulate the archetypes in a P
func getP_A<T: P>(_: T.Type) -> T.A.Type { return T.A.self }
func getP_X<T: P>(_: T.Type) -> T.X.Type { return T.X.self }

// ... which we use to check if the compiler is following through the concrete
// same-type constraints implied by the subprotocols.
func checkQ4_A<T: Q4>(x: T.Type) { sameType(getP_A(x), Int.self) }
func checkQ4_X<T: Q4>(x: T.Type) { sameType(getP_X(x), Int.self) }

// FIXME: these do not work, seemingly mainly due to the 'recursive decl validation'
// FIXME in GenericSignatureBuilder.cpp.
/*
func checkQ5_A<T: Q5>(x: T.Type) { sameType(getP_A(x), Int.self) }
func checkQ5_X<T: Q5>(x: T.Type) { sameType(getP_X(x), Int.self) }
*/


// Typealiases happen to allow imposing same type requirements between parent
// protocols
protocol P6_1 {
    associatedtype A
}
protocol P6_2 {
    associatedtype B
}
protocol Q6: P6_1, P6_2 {
    typealias A = B
}

func getP6_1_A<T: P6_1>(_: T.Type) -> T.A.Type { return T.A.self }
func getP6_2_B<T: P6_2>(_: T.Type) -> T.B.Type { return T.B.self }

func checkQ6<T: Q6>(x: T.Type) {
    sameType(getP6_1_A(x), getP6_2_B(x))
}

