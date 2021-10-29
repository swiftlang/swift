// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: 68084643.(file).R@
// CHECK-NEXT: Requirement signature: <Self where Self.RT : F>
protocol R {
  associatedtype RT: F
}

// CHECK: 68084643.(file).F@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.OT.RT, Self.OT : R>
protocol F {
  associatedtype OT: R where OT.RT == Self
}

// CHECK: 68084643.(file).P@
// CHECK-NEXT: Requirement signature: <Self>
protocol P {
  associatedtype PT
}

// CHECK: 68084643.(file).O@
// CHECK-NEXT: Requirement signature: <Self where Self.O : P>
protocol O {
  associatedtype O: P
}

// CHECK: 68084643.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self.LO : O, Self.OT == Self.LO.O, Self.RT : F, Self.OT.PT == Self.RT.OT>
protocol B {
  associatedtype RT: F
  associatedtype LO: O where LO.O == OT
  associatedtype OT where OT.PT == RT.OT
}

// CHECK: 68084643.(file).Boom@
// CHECK-NEXT: Requirement signature: <Self where Self.D : B, Self.E : Sequence, Self.F : Sequence, Self.E.Element == Self.F.Element>
protocol Boom {
  associatedtype D: B
  associatedtype E: Sequence
  associatedtype F: Sequence where E.Element == F.Element
}
