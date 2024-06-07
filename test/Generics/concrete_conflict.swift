// RUN: %target-swift-frontend -typecheck -verify %s -debug-generic-signatures 2>&1 | %FileCheck %s

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

// expected-error@+3{{no type for 'Self.T.T' can satisfy both 'Self.T.T == String' and 'Self.T.T == Int'}}
// expected-error@+2{{no type for 'Self.T.T' can satisfy both 'Self.T.T == Float' and 'Self.T.T == Int'}}
// expected-error@+1{{no type for 'Self.T.T' can satisfy both 'Self.T.T == Float' and 'Self.T.T == String'}}
protocol P4 {
  associatedtype T : P1 & P2 & P3
}

class Class<T> {}

extension Class where T == Bool {
  // CHECK-LABEL: .badRequirement()@
  // CHECK-NEXT: <T where T == Int>
  func badRequirement() where T == Int { }
  // expected-error@-1 {{no type for 'T' can satisfy both 'T == Int' and 'T == Bool'}}
}
