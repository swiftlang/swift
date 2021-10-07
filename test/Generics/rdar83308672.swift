// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: rdar83308672.(file).A@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.X.T, Self.X : P1, Self.Y : P2>
protocol A {
  associatedtype X : P1
  associatedtype Y : P2
    where X.T == Self
}

// CHECK: rdar83308672.(file).P1@
// CHECK-NEXT: Requirement signature: <Self>
protocol P1 {
  associatedtype T
}

// CHECK: rdar83308672.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.T : B>
protocol P2 {
  associatedtype T : B
}

// CHECK: rdar83308672.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self.X == Self.Y>
protocol B {
  associatedtype X
  associatedtype Y
    where X == Y
}

// Note that T.X == T.Y implies T : B, but also T : B implies T.X == T.Y;
// we can drop one requirement but not both.
//
// If T : B was explicitly stated, we drop T.X == T.Y; otherwise we keep
// T.X == T.Y and drop T : B.

// CHECK: rdar83308672.(file).G1@
// CHECK-NEXT: Requirement signature: <Self where Self.T : A, Self.T.X == Self.T.Y>
protocol G1 {
  associatedtype T : A where T.X == T.Y
}

// CHECK: rdar83308672.(file).G2@
// CHECK-NEXT: Requirement signature: <Self where Self.T : A, Self.T : B>
protocol G2 {
  associatedtype T : A where T : B, T.X == T.Y
}

// CHECK: rdar83308672.(file).G3@
// CHECK-NEXT: Requirement signature: <Self where Self.T : A, Self.T : B>
protocol G3 {
  associatedtype T : A where T.X == T.Y, T : B
}

// CHECK: rdar83308672.(file).G4@
// CHECK-NEXT: Requirement signature: <Self where Self.T : A, Self.T : B>
protocol G4 {
  associatedtype T : A where T : B
}
