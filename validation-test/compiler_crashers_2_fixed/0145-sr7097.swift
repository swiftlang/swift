// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -o -

protocol P1 { }

protocol P2 {
  associatedtype Assoc // expected-note{{'Assoc' declared here}}
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.Assoc == ConformsToP1>
protocol P3 : P2 { }

struct S0<M: P3> where M.Assoc: P1 { } // expected-warning{{redundant conformance constraint 'M.Assoc': 'P1'}}
// expected-note@-1{{conformance constraint 'M.Assoc': 'P1' implied here}}

struct ConformsToP1: P1 { }

extension P3 {
  typealias Assoc = ConformsToP1 // expected-warning{{typealias overriding associated type 'Assoc' from protocol 'P2' is better expressed as same-type constraint on the protocol}}
}

protocol P5 {
}

extension P5 {
  // CHECK-LABEL: P5.testSR7097
  // CHECK-NEXT: Generic signature: <Self, M where Self : P5, M : P3>
  // CHECK-NEXT: <τ_0_0, τ_1_0 where τ_0_0 : P5, τ_1_0 : P3>
  func testSR7097<M>(_: S0<M>.Type) {}
}


