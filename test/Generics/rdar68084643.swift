// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: 68084643.(file).R@
// CHECK-NEXT: Requirement signature: <Self where Self.[R]RT : F>
protocol R {
  associatedtype RT: F
}

// CHECK: 68084643.(file).F@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[F]OT.[R]RT, Self.[F]OT : R>
protocol F {
  associatedtype OT: R where OT.RT == Self
}

// CHECK: 68084643.(file).P@
// CHECK-NEXT: Requirement signature: <Self>
protocol P {
  associatedtype PT
}

// CHECK: 68084643.(file).O@
// CHECK-NEXT: Requirement signature: <Self where Self.[O]O : P>
protocol O {
  associatedtype O: P
}

// CHECK: 68084643.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self.[B]LO : O, Self.[B]OT == Self.[B]LO.[O]O, Self.[B]RT : F, Self.[B]OT.[P]PT == Self.[B]RT.[F]OT>
protocol B {
  associatedtype RT: F
  associatedtype LO: O where LO.O == OT
  associatedtype OT where OT.PT == RT.OT
}

// CHECK: 68084643.(file).Boom@
// CHECK-NEXT: Requirement signature: <Self where Self.[Boom]D : B, Self.[Boom]E : Sequence, Self.[Boom]F : Sequence, Self.[Boom]E.[Sequence]Element == Self.[Boom]F.[Sequence]Element>
protocol Boom {
  associatedtype D: B
  associatedtype E: Sequence
  associatedtype F: Sequence where E.Element == F.Element
}
