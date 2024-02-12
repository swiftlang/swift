// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -debug-generic-signatures -typecheck %s 2>&1 | %FileCheck %s

protocol P24 {
  associatedtype C: P20
}

protocol P20 { }

struct X24<T: P20> : P24 {
  typealias C = T
}

protocol P26 {
  associatedtype C: X3
}

struct X26<T: X3> : P26 {
  typealias C = T
}

class X3 { }

// CHECK-LABEL: .P25a@
// CHECK-NEXT: Requirement signature: <Self where Self.[P25a]A == X24<Self.[P25a]B>, Self.[P25a]B : P20>
protocol P25a {
  associatedtype A: P24
  associatedtype B: P20 where A == X24<B>
}

// CHECK-LABEL: .P25b@
// CHECK-NEXT: Requirement signature: <Self where Self.[P25b]A == X24<Self.[P25b]B>, Self.[P25b]B : P20>
protocol P25b {
  associatedtype A
  associatedtype B: P20 where A == X24<B>
}

// CHECK-LABEL: .P27a@
// CHECK-NEXT: Requirement signature: <Self where Self.[P27a]A == X26<Self.[P27a]B>, Self.[P27a]B : X3>
protocol P27a {
  associatedtype A: P26

  associatedtype B: X3 where A == X26<B>
}

// CHECK-LABEL: .P27b@
// CHECK-NEXT: Requirement signature: <Self where Self.[P27b]A == X26<Self.[P27b]B>, Self.[P27b]B : X3>
protocol P27b {
  associatedtype A
  associatedtype B: X3 where A == X26<B>
}
