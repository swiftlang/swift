// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -verify -typecheck %s -debug-generic-signatures \
// RUN:   -debug-inverse-requirements 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: swift_feature_SuppressedAssociatedTypes

// CHECK-LABEL: .P2@
// CHECK: <Self where Self : Copyable, Self : Escapable, Self.[P2]A : Copyable, Self.[P2]A : Escapable>
protocol P2 { associatedtype A }

// CHECK-LABEL: .P2_IC@
// CHECK: <Self where Self : Escapable, Self.[P2_IC]A : Copyable, Self.[P2_IC]A : Escapable>
protocol P2_IC: ~Copyable { associatedtype A }

// CHECK-LABEL: .P2_CI@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[P2_CI]A : Escapable>
protocol P2_CI { associatedtype A: ~Copyable }

// CHECK-LABEL: .P2_II@
// CHECK: Requirement signature: <Self where Self : Escapable, Self.[P2_II]A : Escapable>
protocol P2_II: ~Copyable { associatedtype A: ~Copyable }

// CHECK-LABEL: .P3@
// CHECK: Requirement signature: <Self where Self.[P3]B : Copyable>
protocol P3 where Self: (~Copyable & ~Escapable) { associatedtype B: ~Escapable }

// CHECK-LABEL: .P4@
// CHECK: Requirement signature: <Self where Self : Copyable, Self.[P4]B : Copyable, Self.[P4]C : Escapable>
protocol P4<B, D>: ~Escapable {
  associatedtype B: ~Escapable
  associatedtype C: ~Copyable
  associatedtype D: ~Escapable, ~Copyable
}

// CHECK-LABEL: .test3@
// CHECK-NEXT: Generic signature: <T where T : Escapable, T : P4>
func test3<T>(_ p: T) where T: P4 {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P4
// CHECK-NEXT: Generic signature: <Self where Self : Escapable, Self : P4>
extension P4 {}

// CHECK-LABEL: .Explicit@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[Explicit]Elm : Copyable, Self.[Explicit]Elm : Escapable>
protocol Explicit: Copyable, Escapable {
  associatedtype Elm: Copyable, Escapable
}
