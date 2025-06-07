// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: rdar83308672.(file).A@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[A]X.[P1]T, Self.[A]X : P1, Self.[A]Y : P2>
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
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T : B>
protocol P2 {
  associatedtype T : B
}

// CHECK: rdar83308672.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self.[B]X == Self.[B]Y>
protocol B {
  associatedtype X
  associatedtype Y
    where X == Y
}

// Note that T.X == T.Y implies T : B, but also T : B implies T.X == T.Y;
// we can drop one requirement but not both.

// CHECK: rdar83308672.(file).G1@
// CHECK-NEXT: Requirement signature: <Self where Self.[G1]T : A, Self.[G1]T.[A]X == Self.[G1]T.[A]Y>
protocol G1 {
  associatedtype T : A where T.X == T.Y
}

// CHECK: rdar83308672.(file).G2@
// CHECK-NEXT: Requirement signature: <Self where Self.[G2]T : A, Self.[G2]T.[A]X == Self.[G2]T.[A]Y>
protocol G2 {
  associatedtype T : A where T : B, T.X == T.Y
}

// CHECK: rdar83308672.(file).G3@
// CHECK-NEXT: Requirement signature: <Self where Self.[G3]T : A, Self.[G3]T.[A]X == Self.[G3]T.[A]Y>
protocol G3 {
  associatedtype T : A where T.X == T.Y, T : B
}

// CHECK: rdar83308672.(file).G4@
// CHECK-NEXT: Requirement signature: <Self where Self.[G4]T : A, Self.[G4]T.[A]X == Self.[G4]T.[A]Y>
protocol G4 {
  associatedtype T : A where T : B
}
