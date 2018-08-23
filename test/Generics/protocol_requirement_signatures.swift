// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -debug-generic-signatures > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0>
protocol P1 {}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0>
protocol P2 {}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0>
protocol P3 {}

// basic protocol
// CHECK-LABEL: .Q1@
// CHECK-NEXT: Requirement signature: <Self where Self.X : P1>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0.X : P1>
protocol Q1 {
    associatedtype X: P1 // expected-note {{declared here}}
}

// inheritance
// CHECK-LABEL: .Q2@
// CHECK-NEXT: Requirement signature: <Self where Self : Q1>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q1>
protocol Q2: Q1 {}

// inheritance without any new requirements
// CHECK-LABEL: .Q3@
// CHECK-NEXT: Requirement signature: <Self where Self : Q1>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q1>
protocol Q3: Q1 {
    associatedtype X
}

// inheritance adding a new conformance
// CHECK-LABEL: .Q4@
// CHECK-NEXT: Requirement signature: <Self where Self : Q1, Self.X : P2>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q1, τ_0_0.X : P2>
protocol Q4: Q1 {
    associatedtype X: P2 // expected-warning{{redeclaration of associated type 'X'}}
                   // expected-note@-1 2{{'X' declared here}}
}

// multiple inheritance
// CHECK-LABEL: .Q5@
// CHECK-NEXT: Requirement signature: <Self where Self : Q2, Self : Q3, Self : Q4>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0 : Q3, τ_0_0 : Q4>
protocol Q5: Q2, Q3, Q4 {}

// multiple inheritance without any new requirements
// CHECK-LABEL: .Q6@
// CHECK-NEXT: Requirement signature: <Self where Self : Q2, Self : Q3, Self : Q4>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0 : Q3, τ_0_0 : Q4>
protocol Q6: Q2, // expected-note{{conformance constraint 'Self.X': 'P1' implied here}}
             Q3, Q4 {
    associatedtype X: P1 // expected-warning{{redundant conformance constraint 'Self.X': 'P1'}}
                   // expected-warning@-1{{redeclaration of associated type 'X' from protocol 'Q4' is}}
}

// multiple inheritance with a new conformance
// CHECK-LABEL: .Q7@
// CHECK-NEXT: Requirement signature: <Self where Self : Q2, Self : Q3, Self : Q4, Self.X : P3>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0 : Q2, τ_0_0 : Q3, τ_0_0 : Q4, τ_0_0.X : P3>
protocol Q7: Q2, Q3, Q4 {
    associatedtype X: P3 // expected-warning{{redeclaration of associated type 'X'}}
}

// SR-5945
class SomeBaseClass {}

// CHECK-DAG: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.BType.AType, Self.BType : P5, Self.Native : SomeBaseClass>
protocol P4 {
	associatedtype Native : SomeBaseClass
  associatedtype BType : P5 where BType.AType == Self
}

// CHECK-DAG: .P5@
// CHECK-NEXT: <Self where Self == Self.AType.BType, Self.AType : P4>
protocol P5 {
	associatedtype AType : P4 where AType.BType == Self
}

// SR-8119
protocol P6 {
  associatedtype A1: P7
}

// CHECK-DAG: .P7@
// CHECK-NEXT: <Self where Self == Self.A2.A1, Self.A2 : P6>
protocol P7 {
  associatedtype A2: P6 where A2.A1 == Self
}
