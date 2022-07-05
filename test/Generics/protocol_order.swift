// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T
}

protocol P2 {
  associatedtype T
}

protocol P3 : P2 {
  associatedtype T
}

protocol P4 {}

// CHECK-LABEL: protocol_order.(file).P5@
// CHECK-LABEL: Requirement signature: <Self where Self.[P5]T : P1, Self.[P5]T : P3, Self.[P5]T.[P1]T : P4>
protocol P5 {
  associatedtype T : P1 & P3 where T.T : P4
}
