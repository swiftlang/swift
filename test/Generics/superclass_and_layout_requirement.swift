// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class C {}

// CHECK-LABEL: superclass_and_layout_requirement.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.[P1]T : C>
protocol P1 {
  associatedtype T : C
}

// CHECK-LABEL: superclass_and_layout_requirement.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.[P2]T : C>
protocol P2 {
  associatedtype T where T : C, T : AnyObject
}

// CHECK-LABEL: superclass_and_layout_requirement.(file).P3a@
// CHECK-LABEL: Requirement signature: <Self where Self.[P3a]T : P1>
protocol P3a {
  associatedtype T : P1 where T.T : AnyObject
}

// CHECK-LABEL: superclass_and_layout_requirement.(file).P4a@
// CHECK-LABEL: Requirement signature: <Self where Self.[P4a]T : AnyObject>
protocol P4a {
  associatedtype T : AnyObject
}

// CHECK-LABEL: superclass_and_layout_requirement.(file).P5a@
// CHECK-LABEL: Requirement signature: <Self where Self.[P5a]T : P4a, Self.[P5a]T.[P4a]T : C>
protocol P5a {
  associatedtype T where T : P4a, T.T : C
}
