// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]T : C>
protocol P {
  typealias A = C

  associatedtype T : A
}

class C {}

protocol PBad {
  typealias A = C

  associatedtype B : P

  associatedtype T1 : B.A
  // expected-error@-1 {{type 'Self.T1' constrained to non-protocol, non-class type 'Self.B.A'}}

  associatedtype T2 where T2 : A
}

protocol PWorse {
  typealias A = C

  associatedtype T : Self.A
}

protocol Q1 {}
protocol Q2 {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P
// CHECK-NEXT: Generic signature: <Self where Self : P>
extension P {
  typealias B = (Q1 & Q2)
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P
// CHECK-NEXT: Generic signature: <Self where Self : P, Self : Q1, Self : Q2>
extension P where Self: B {}
