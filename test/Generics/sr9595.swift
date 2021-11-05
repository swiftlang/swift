// RUN: %target-swift-frontend -debug-generic-signatures -requirement-machine-protocol-signatures=on -typecheck %s 2>&1 | %FileCheck %s

protocol R {}

protocol Q {
  associatedtype Assoc
}

// CHECK-LABEL: sr9595.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.T == Self.U.U, Self.U : P1, Self.U : Q, Self.U == Self.T.U, Self.U.Assoc : R>
protocol P1 {
  associatedtype T where T == U.U
  associatedtype U : P1, Q where U.Assoc : R, U == T.U
}

// CHECK-LABEL: sr9595.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.T == Self.U.U, Self.U : P2, Self.U : Q, Self.U == Self.T.U, Self.U.Assoc : R>
protocol P2 {
  associatedtype T : P2 where T == U.U
  associatedtype U : P2, Q where U.Assoc : R, U == T.U
}

// CHECK-LABEL: sr9595.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.T : Q, Self.T == Self.U.U, Self.U : P3, Self.U == Self.T.U, Self.T.Assoc : R>
protocol P3 {
  associatedtype T : Q where T.Assoc : R, T == U.U
  associatedtype U : P3 where U == T.U
}

// CHECK-LABEL: sr9595.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self.T : Q, Self.T == Self.U.U, Self.U : P4, Self.U == Self.T.U, Self.T.Assoc : R>
protocol P4 {
  associatedtype T : P4, Q where T.Assoc : R, T == U.U
  associatedtype U : P4 where U.Assoc : R, U == T.U
}
