// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class A {}
class B : A {}
class C : B {}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pabc@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pabc]T : C>
protocol Pabc {
  associatedtype T where T : A, T : B, T : C
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pacb@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pacb]T : C>
protocol Pacb {
  associatedtype T where T : A, T : C, T : B
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pbac@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pbac]T : C>
protocol Pbac {
  associatedtype T where T : B, T : A, T : C
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pbca@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pbca]T : C>
protocol Pbca {
  associatedtype T where T : B, T : C, T : A
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pcab@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pcab]T : C>
protocol Pcab {
  associatedtype T where T : C, T : A, T : B
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).Pcba@
// CHECK-NEXT: Requirement signature: <Self where Self.[Pcba]T : C>
protocol Pcba {
  associatedtype T where T : C, T : B, T : A
}

protocol Pa {
  associatedtype T where T : A
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).PaQc@
// CHECK-NEXT: Requirement signature: <Self where Self.[PaQc]T : Pa, Self.[PaQc]T.[Pa]T : C>
protocol PaQc {
  associatedtype T where T : Pa, T.T : C
}

protocol Pc {
  associatedtype T where T : C
}

// CHECK-LABEL: minimize_superclass_unification_non_generic.(file).PcQa@
// CHECK-NEXT: Requirement signature: <Self where Self.[PcQa]T : Pc>
protocol PcQa {
  associatedtype T where T : Pc, T.T : A
}