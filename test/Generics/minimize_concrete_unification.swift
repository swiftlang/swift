// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<T> {}

// Three requirements where any two imply the third:
//
// a) T == G<U>
// b) T == G<Int>
// c) U == Int

// CHECK-LABEL: minimize_concrete_unification.(file).Pab@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pab]T == G<Int>, Self.[Pab]U == Int>

protocol Pab {
  associatedtype T where T == G<U>, T == G<Int>
  associatedtype U
}

// CHECK-LABEL: minimize_concrete_unification.(file).Pac@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pac]T == G<Int>, Self.[Pac]U == Int>

protocol Pac {
  associatedtype T where T == G<U>
  associatedtype U where U == Int
}

// CHECK-LABEL: minimize_concrete_unification.(file).Pbc@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pbc]T == G<Int>, Self.[Pbc]U == Int>

protocol Pbc {
  associatedtype T where T == G<Int>
  associatedtype U where U == Int
}

// CHECK-LABEL: minimize_concrete_unification.(file).Pabc@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pabc]T == G<Int>, Self.[Pabc]U == Int>

protocol Pabc {
  associatedtype T where T == G<U>, T == G<Int>
  associatedtype U where U == Int
}

//

// CHECK-LABEL: minimize_concrete_unification.(file).Pa@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pa]T == G<Self.[Pa]U>>

protocol Pa {
  associatedtype T where T == G<U>
  associatedtype U
}

// CHECK-LABEL: minimize_concrete_unification.(file).PaQb@
// CHECK-NEXT: Requirement signature: <Self where Self.[PaQb]X : Pa, Self.[PaQb]X.[Pa]U == Int>

protocol PaQb {
  associatedtype X : Pa where X.T == G<Int>
}

// CHECK-LABEL: minimize_concrete_unification.(file).PaQc@
// CHECK-NEXT: Requirement signature: <Self where Self.[PaQc]X : Pa, Self.[PaQc]X.[Pa]U == Int>

protocol PaQc {
  associatedtype X : Pa where X.U == Int
}

//

// CHECK-LABEL: minimize_concrete_unification.(file).Pb@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pb]T == G<Int>>

protocol Pb {
  associatedtype T where T == G<Int>
  associatedtype U
}

// CHECK-LABEL: minimize_concrete_unification.(file).PbQa@
// CHECK-NEXT: Requirement signature: <Self where Self.[PbQa]X : Pb, Self.[PbQa]X.[Pb]U == Int>

protocol PbQa {
  associatedtype X : Pb where X.T == G<X.U>
}

// CHECK-LABEL: minimize_concrete_unification.(file).PbQc@
// CHECK-NEXT: Requirement signature: <Self where Self.[PbQc]X : Pb, Self.[PbQc]X.[Pb]U == Int>

protocol PbQc {
  associatedtype X : Pb where X.U == Int
}

//

// CHECK-LABEL: minimize_concrete_unification.(file).Pc@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pc]U == Int>

protocol Pc {
  associatedtype T
  associatedtype U where U == Int
}

// CHECK-LABEL: minimize_concrete_unification.(file).PcQa@
// CHECK-NEXT: Requirement signature: <Self where Self.[PcQa]X : Pc, Self.[PcQa]X.[Pc]T == G<Int>>

protocol PcQa {
  associatedtype X : Pc where X.T == G<X.U>
}

// CHECK-LABEL: minimize_concrete_unification.(file).PcQb@
// CHECK-NEXT: Requirement signature: <Self where Self.[PcQb]X : Pc, Self.[PcQb]X.[Pc]T == G<Int>>

protocol PcQb {
  associatedtype X : Pc where X.T == G<Int>
}

//

// CHECK-LABEL: minimize_concrete_unification.(file).Q1@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q1]V : Pa, Self.[Q1]W == Self.[Q1]V.[Pa]U>

protocol Q1 {
  associatedtype V where V : Pa, V.T == G<W>
  associatedtype W
}

//

// CHECK-LABEL: minimize_concrete_unification.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T == G<Self.[P1]U>>

protocol P1 {
  associatedtype T
  associatedtype U where T == G<U>
}

// CHECK-LABEL: minimize_concrete_unification.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T == G<Int>>

protocol P2 {
  associatedtype T where T == G<Int>
  associatedtype U
}

// CHECK-LABEL: minimize_concrete_unification.(file).P@
// CHECK-NEXT: Requirement signature: <Self>

protocol P {
  associatedtype T
  associatedtype U
}

// CHECK-LABEL: minimize_concrete_unification.(file).R1@
// CHECK-NEXT: Requirement signature: <Self where Self.[R1]X : P, Self.[R1]X : Pa, Self.[R1]X.[P]U == Int>

protocol R1 {
  // The GSB would drop 'X.T == Int' from the minimal signature.
  associatedtype X where X : P, X.T == G<Int>, X : Pa
}

// CHECK-LABEL: minimize_concrete_unification.(file).R2@
// CHECK-NEXT: Requirement signature: <Self where Self.[R2]X : P, Self.[R2]X : Pb, Self.[R2]X.[P]U == Int>

protocol R2 {
  // The GSB would drop 'X.T == Int' from the minimal signature.
  associatedtype X where X : P, X.T == G<X.U>, X : Pb
}

// CHECK-LABEL: minimize_concrete_unification.(file).R3@
// CHECK-NEXT: Requirement signature: <Self where Self.[R3]X : Pa, Self.[R3]X : Pb>

protocol R3 {
  // The GSB would include a redundant 'X.T == Int' in the minimal signature.
  associatedtype X where X : Pa, X.T == G<Int>, X : Pb
}
