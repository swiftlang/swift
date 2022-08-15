// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<T> {}

protocol P {
  associatedtype T where T == G<X>

  associatedtype X
}

protocol Q {
  associatedtype T where T == G<Y>

  associatedtype Y
}

// Make sure that substitution simplification records a projection path for
// the induced rule.

// CHECK-LABEL: simplify_concrete_substitutions_projection.(file).R1@
// CHECK-NEXT: Requirement signature: <Self where Self.[R1]T : P, Self.[R1]T : Q>

protocol R1 {
  associatedtype T where T : P, T : Q
}

// CHECK-LABEL: simplify_concrete_substitutions_projection.(file).R2@
// CHECK-NEXT: Requirement signature: <Self where Self.[R2]T : P, Self.[R2]T : Q>

protocol R2 {
  associatedtype T where T : P, T : Q, T.X == T.Y
}