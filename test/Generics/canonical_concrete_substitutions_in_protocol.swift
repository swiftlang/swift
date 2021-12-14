// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

struct G<T> {}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.A == G<Self.B>, Self.B == Self.T.X, Self.T : Q>

protocol P {
  associatedtype A where A == G<T.X>
  associatedtype B where B == T.X
  associatedtype T : Q
}

protocol Q {
  associatedtype X
}

protocol QQ : Q {}

protocol R {
  associatedtype A
  associatedtype C: QQ where C.X == G<A>
}