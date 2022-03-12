// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T where T == Int
}

protocol P2 {
  associatedtype T where T == String
}

protocol P3 {
  associatedtype T where T == Float
}

// We end up with two redundant rules:
//
// [P4:T].[P1:T].[concrete: String]
// [P4:T].[P1:T].[concrete: Float]
//
// These rules are unordered with respect to each other because concrete type
// symbols are incomparable, but they conflict so we should not compare them;
// just make sure we don't crash.

// CHECK-LABEL: .P4@
// CHECK-LABEL: Requirement signature: <Self where Self.[P4]T : P1, Self.[P4]T : P2, Self.[P4]T : P3>

protocol P4 {
  associatedtype T : P1 & P2 & P3
  // expected-note@-1 2{{same-type constraint 'Self.T.T' == 'Int' implied here}}
  // expected-error@-2 {{'Self.T.T' cannot be equal to both 'String' and 'Int'}}
  // expected-error@-3 {{'Self.T.T' cannot be equal to both 'Float' and 'Int'}}
}

class Class<T> {}

extension Class where T == Bool {
  // CHECK-LABEL: .badRequirement()@
  // CHECK-NEXT: <T where T == Bool>
  func badRequirement() where T == Int { }
  // expected-error@-1 {{generic parameter 'T' cannot be equal to both 'Int' and 'Bool'}}
}