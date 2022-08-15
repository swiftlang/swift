// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {}
class C : P {}

// CHECK-LABEL: redundant_protocol_inheritance_via_concrete.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self : C>
protocol Q : P, C {}
