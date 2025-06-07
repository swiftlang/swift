// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction -dump-requirement-machine 2>&1 | %FileCheck %s --check-prefix=RULE

protocol P {}

class Base : P {}

class Derived : Base {}

struct G<X> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X where X == Derived>
extension G where X : Base, X : P, X == Derived {}

// RULE: + (requirement "τ_0_0" subclass_of "Base")
// RULE: + (requirement "τ_0_0" conforms_to "P")
// RULE: + (requirement "τ_0_0" same_type "Derived")

// RULE: Rewrite system: {
// RULE-NEXT: - [Copyable].[Copyable] => [Copyable] [permanent]
// RULE-NEXT: - [Escapable].[Escapable] => [Escapable] [permanent]
// RULE-NEXT: - [P].[P] => [P] [permanent]
// RULE-NEXT: - [P].[Copyable] => [P] [explicit]
// RULE-NEXT: - [P].[Escapable] => [P] [explicit]
// RULE-NEXT: - τ_0_0.[Copyable] => τ_0_0 [explicit]
// RULE-NEXT: - τ_0_0.[Escapable] => τ_0_0 [explicit]
// RULE-NEXT: - τ_0_0.[superclass: Base] => τ_0_0 [explicit]
// RULE-NEXT: - τ_0_0.[P] => τ_0_0 [explicit]
// RULE-NEXT: - τ_0_0.[concrete: Derived] => τ_0_0 [explicit]
// RULE-NEXT: - τ_0_0.[layout: _NativeClass] => τ_0_0
// RULE-NEXT: - τ_0_0.[superclass: Derived] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Derived : Copyable] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Derived : Escapable] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Derived : P] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Base : Copyable] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Base : Escapable] => τ_0_0
// RULE-NEXT: - τ_0_0.[concrete: Base : P] => τ_0_0
// RULE-NEXT: }

// Notes:
//
// - concrete contraction kills the 'X : P' requirement before building the
//   rewrite system so this test passes trivially.
//
// - without concrete contraction, we used to hit a problem where the sort
//   in the minimal conformances algorithm would hit the unordered concrete
//   conformances [concrete: Derived : P] vs [concrete: Base : P].
