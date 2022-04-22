// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T : P1, Self.[P1]U == Int, Self.[P1]V == Int>
protocol P1 {
  associatedtype T : P1
  associatedtype U where U == Int
  associatedtype V where V == T.U
}

struct G<X> {}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T : P2, Self.[P2]U == G<Self.[P2]X>, Self.[P2]V == G<Self.[P2]T.[P2]X>>
protocol P2 {
  associatedtype T : P2
  associatedtype U where U == G<X>
  associatedtype V where V == T.U
  associatedtype X
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]T : P3, Self.[P3]U == G<Self.[P3]X>, Self.[P3]V == G<Self.[P3]X>, Self.[P3]X == Self.[P3]T.[P3]X>
protocol P3 {
  associatedtype T : P3
  associatedtype U where U == G<X>
  associatedtype V where V == T.U, V == G<X>
  associatedtype X
}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]T : P4, Self.[P4]U == G<Self.[P4]X>, Self.[P4]V == G<Self.[P4]X>, Self.[P4]X == Self.[P4]T.[P4]X>
protocol P4 {
  associatedtype T : P4
  associatedtype U where U == G<X>
  associatedtype V where V == T.U
  associatedtype X where X == T.X
}

// We don't split concrete equivalence classes if the signature had an error,
// but we also shouldn't crash in verify() because of an unsplit concrete
// equivalence class.

// CHECK-LABEL: .P4Bad@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4Bad]T : P4Bad, Self.[P4Bad]U == G<Self.[P4Bad]X>, Self.[P4Bad]V == Self.[P4Bad]T.[P4Bad]U>
protocol P4Bad {
  associatedtype T : P4Bad
  associatedtype U where U == G<X>
  associatedtype V where V == T.U
  associatedtype X where X == U.X
}
