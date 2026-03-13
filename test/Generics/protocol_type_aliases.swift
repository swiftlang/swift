// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures > %t.dump 2>&1


func sameType<T>(_: T.Type, _: T.Type) {}

protocol P {
    associatedtype A // expected-note{{'A' declared here}}
    typealias X = A
}
protocol Q {
    associatedtype B: P
}

// CHECK-LABEL: .requirementOnNestedTypeAlias@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q, τ_0_0.[Q]B.[P]A == Int>
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
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.[Q2]C == S<τ_0_0.[Q2]B.[P2]A>>
func requirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, T.C == T.B.X {}

// CHECK-LABEL: .concreteRequirementOnConcreteNestedTypeAlias@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0.[Q2]C == τ_0_0.[Q2]B.[P2]A>
func concreteRequirementOnConcreteNestedTypeAlias<T>(_: T) where T: Q2, S<T.C> == T.B.X {}


// Incompatible concrete typealias types are flagged as such
protocol P3 {
    typealias T = Int
}
protocol Q3: P3 { // expected-error{{no type for 'Self.T' can satisfy both 'Self.T == Int' and 'Self.T == Float'}}
    typealias T = Float
}

protocol P3_1 {
    typealias T = Float
}
protocol Q3_1: P3, P3_1 {} // expected-error{{no type for 'Self.T' can satisfy both 'Self.T == Float' and 'Self.T == Int'}}


// Subprotocols can force associated types in their parents to be concrete, and
// this should be understood for types constrained by the subprotocols.

// CHECK-LABEL: .Q4@
// CHECK: Requirement signature: <Self where Self : P, Self.[P]A == Int>
protocol Q4: P {
    typealias A = Int // expected-warning{{typealias overriding associated type 'A' from protocol 'P'}}
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

func checkQ5_A<T: Q5>(x: T.Type) { sameType(getP_A(x), Int.self) }
func checkQ5_X<T: Q5>(x: T.Type) { sameType(getP_X(x), Int.self) }



// Typealiases allow imposing same type requirements between parent protocols
protocol P6_1 {
    associatedtype A // expected-note{{'A' declared here}}
}
protocol P6_2 {
    associatedtype B
}

// CHECK-LABEL: .Q6@
// CHECK-NEXT: Requirement signature: <Self where Self : P6_1, Self : P6_2, Self.[P6_1]A == Self.[P6_2]B>
protocol Q6: P6_1, P6_2 {
    typealias A = B // expected-warning{{typealias overriding associated type}}
}

func getP6_1_A<T: P6_1>(_: T.Type) -> T.A.Type { return T.A.self }
func getP6_2_B<T: P6_2>(_: T.Type) -> T.B.Type { return T.B.self }

func checkQ6<T: Q6>(x: T.Type) {
    sameType(getP6_1_A(x), getP6_2_B(x))
}

protocol P7 {
  typealias A = Int
}

// CHECK-LABEL: .P7a@
// CHECK: Requirement signature: <Self where Self : P7, Self.[P7a]A == Int>
protocol P7a : P7 {
  associatedtype A   // expected-warning{{associated type 'A' is redundant with type 'A' declared in inherited protocol 'P7'}}
}

func testP7A<T : P7a>(_: T, a: T.A) -> Int { return a }
