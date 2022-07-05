// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol Sequence {}

protocol Collection : Sequence {}

struct MyCollection : Collection {}

// CHECK-LABEL: inherited_concrete_conformance_in_protocol.(file).P@
// CHECK-LABEL: Requirement signature: <Self where Self.[P]T == MyCollection>

protocol P {
  associatedtype T : Collection where T == MyCollection
}
