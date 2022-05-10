// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class A<T> {}
class B<T> : A<T> {}
class C<T, U> : A<(T, U)> {}
class D<T, U> : A<U> {}

// ----------------

protocol P1 {
  associatedtype T where T : A<X>

  associatedtype X
}

protocol Q1 {
  associatedtype T where T : B<Y>

  associatedtype Y
}

// CHECK-LABEL: .R1a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R1a]T : P1, Self.[R1a]T : Q1>

protocol R1a {
  associatedtype T where T : P1, T : Q1, T.X == T.Y
}

// CHECK-LABEL: .R1b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R1b]T : P1, Self.[R1b]T : Q1>

protocol R1b {
  associatedtype T where T : P1, T : Q1
}

// ----------------

protocol P2 {
  associatedtype T where T : B<X>

  associatedtype X
}

protocol Q2 {
  associatedtype T where T : A<Y>

  associatedtype Y
}

// CHECK-LABEL: .R2a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R2a]T : P2, Self.[R2a]T : Q2>

protocol R2a {
  associatedtype T where T : P2, T : Q2, T.X == T.Y
}

// CHECK-LABEL: .R2b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R2b]T : P2, Self.[R2b]T : Q2>

protocol R2b {
  associatedtype T where T : P2, T : Q2
}

// ----------------

protocol P3 {
  associatedtype T where T : A<X>

  associatedtype X
}

protocol Q3 {
  associatedtype T where T : C<Y, Z>

  associatedtype Y
  associatedtype Z
}

// CHECK-LABEL: .R3a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R3a]T : P3, Self.[R3a]T : Q3>

protocol R3a {
  associatedtype T where T : P3, T : Q3, T.X == (T.Y, T.Z)
}

// CHECK-LABEL: .R3b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R3b]T : P3, Self.[R3b]T : Q3>

protocol R3b {
  associatedtype T where T : P3, T : Q3
}

// ----------------

protocol P4 {
  associatedtype T where T : C<X, Y>

  associatedtype X
  associatedtype Y
}

protocol Q4 {
  associatedtype T where T : A<Z>

  associatedtype Z
}

// CHECK-LABEL: .R4a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R4a]T : P4, Self.[R4a]T : Q4>

protocol R4a {
  associatedtype T where T : P4, T : Q4, T.Z == (T.X, T.Y)
}

// CHECK-LABEL: .R4b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R4b]T : P4, Self.[R4b]T : Q4>

protocol R4b {
  associatedtype T where T : P4, T : Q4
}

// ----------------

protocol P5 {
  associatedtype T where T : A<X>

  associatedtype X
}

protocol Q5 {
  associatedtype T where T : D<Y, Z>

  associatedtype Y
  associatedtype Z
}

// CHECK-LABEL: .R5a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R5a]T : P5, Self.[R5a]T : Q5>

protocol R5a {
  associatedtype T where T : P5, T : Q5, T.X == T.Z
}

// CHECK-LABEL: .R5b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R5b]T : P5, Self.[R5b]T : Q5>

protocol R5b {
  associatedtype T where T : P5, T : Q5
}

// ----------------

protocol P6 {
  associatedtype T where T : D<X, Y>

  associatedtype X
  associatedtype Y
}

protocol Q6 {
  associatedtype T where T : A<Z>

  associatedtype Z
}

// CHECK-LABEL: .R6a@
// CHECK-NEXT: Requirement signature: <Self where Self.[R6a]T : P6, Self.[R6a]T : Q6>

protocol R6a {
  associatedtype T where T : P6, T : Q6, T.Z == T.Y
}

// CHECK-LABEL: .R6b@
// CHECK-NEXT: Requirement signature: <Self where Self.[R6b]T : P6, Self.[R6b]T : Q6>

protocol R6b {
  associatedtype T where T : P6, T : Q6
}

// ----------------

// CHECK-LABEL: .P7@
// CHECK-NEXT: Requirement signature: <Self where Self.[P7]V : P1, Self.[P7]W == Self.[P7]V.[P1]X>
protocol P7 {
  associatedtype V where V : P1, V.T : A<W>
  associatedtype W
}