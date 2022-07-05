// RUN: %target-swift-frontend -debug-generic-signatures -typecheck %s 2>&1 | %FileCheck %s

protocol R {}

protocol Q {
  associatedtype Assoc
}

// CHECK-LABEL: sr9595.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.[P1]T == Self.[P1]U.[P1]U, Self.[P1]U : P1, Self.[P1]U : Q, Self.[P1]U == Self.[P1]T.[P1]U, Self.[P1]U.[Q]Assoc : R>
protocol P1 {
  associatedtype T where T == U.U
  associatedtype U : P1, Q where U.Assoc : R, U == T.U
}

// CHECK-LABEL: sr9595.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.[P2]T == Self.[P2]U.[P2]U, Self.[P2]U : P2, Self.[P2]U : Q, Self.[P2]U == Self.[P2]T.[P2]U, Self.[P2]U.[Q]Assoc : R>
protocol P2 {
  associatedtype T : P2 where T == U.U
  associatedtype U : P2, Q where U.Assoc : R, U == T.U
}

// CHECK-LABEL: sr9595.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.[P3]T : Q, Self.[P3]T == Self.[P3]U.[P3]U, Self.[P3]U : P3, Self.[P3]U == Self.[P3]T.[P3]U, Self.[P3]T.[Q]Assoc : R>
protocol P3 {
  associatedtype T : Q where T.Assoc : R, T == U.U
  associatedtype U : P3 where U == T.U
}

// CHECK-LABEL: sr9595.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self.[P4]T : Q, Self.[P4]T == Self.[P4]U.[P4]U, Self.[P4]U : P4, Self.[P4]U == Self.[P4]T.[P4]U, Self.[P4]T.[Q]Assoc : R>
protocol P4 {
  associatedtype T : P4, Q where T.Assoc : R, T == U.U
  associatedtype U : P4 where U.Assoc : R, U == T.U
}
