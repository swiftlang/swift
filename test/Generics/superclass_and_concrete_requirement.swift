// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class C {}

struct S {}

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T == C>
protocol P1 {
  associatedtype T where T : C, T == C
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T : C, Self.[P2]T == S>
protocol P2 {
// expected-error@-1 {{no type for 'Self.T' can satisfy both 'Self.T : C' and 'Self.T == S'}}
// expected-error@-2 {{no type for 'Self.T' can satisfy both 'Self.T : _NativeClass' and 'Self.T == S'}}
  associatedtype T where T : C, T == S
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]T : C, Self.[P3]T == S>
protocol P3 {
// expected-error@-1 {{no type for 'Self.T' can satisfy both 'Self.T : C' and 'Self.T == S'}}
// expected-error@-2 {{no type for 'Self.T' can satisfy both 'Self.T : _NativeClass' and 'Self.T == S'}}
  associatedtype T where T == S, T : C
}

protocol P4a {
  associatedtype T : C
}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]T : P4, Self.[P4]T.[P4]T == S>
protocol P4 {
// expected-error@-1 {{no type for 'Self.T.T' can satisfy both 'Self.T.T == S' and 'Self.T.T : P4'}}
  associatedtype T : P4 where T.T == S
}

class D {}

// CHECK-LABEL: .P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]T == D>
protocol P5 {
// expected-error@-1 {{no type for 'Self.T' can satisfy both 'Self.T : D' and 'Self.T : C'}}
  associatedtype T where T : C, T == D
}
