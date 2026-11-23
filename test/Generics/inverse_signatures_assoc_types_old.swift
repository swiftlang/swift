// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -verify -typecheck %s -debug-generic-signatures \
// RUN:   -debug-inverse-requirements 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: swift_feature_SuppressedAssociatedTypes

// CHECK-LABEL: .P2@
// CHECK: <Self where Self : Copyable, Self : Escapable, Self : Transferable, Self.[P2]A : Copyable, Self.[P2]A : Escapable, Self.[P2]A : Transferable>
protocol P2 { associatedtype A }

// CHECK-LABEL: .P2_IC@
// CHECK: <Self where Self : Deinitable, Self : Escapable, Self : Transferable, Self.[P2_IC]A : Copyable, Self.[P2_IC]A : Escapable, Self.[P2_IC]A : Transferable>
protocol P2_IC: ~Copyable { associatedtype A }

// CHECK-LABEL: .P2_CI@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self : Transferable, Self.[P2_CI]A : Deinitable, Self.[P2_CI]A : Escapable, Self.[P2_CI]A : Transferable>
protocol P2_CI { associatedtype A: ~Copyable } // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}

// CHECK-LABEL: .P2_II@
// CHECK: Requirement signature: <Self where Self : Deinitable, Self : Escapable, Self : Transferable, Self.[P2_II]A : Deinitable, Self.[P2_II]A : Escapable, Self.[P2_II]A : Transferable>
protocol P2_II: ~Copyable { associatedtype A: ~Copyable } // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}

// CHECK-LABEL: .P3@
// CHECK: Requirement signature: <Self where Self : Deinitable, Self : Transferable, Self.[P3]B : Copyable, Self.[P3]B : Transferable>
protocol P3 where Self: (~Copyable & ~Escapable) { associatedtype B: ~Escapable }  // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}

// CHECK-LABEL: .P4@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Transferable, Self.[P4]B : Copyable, Self.[P4]B : Transferable, Self.[P4]C : Deinitable, Self.[P4]C : Escapable, Self.[P4]C : Transferable, Self.[P4]D : Deinitable, Self.[P4]D : Transferable>
protocol P4<B, D>: ~Escapable {
  associatedtype B: ~Escapable  // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
  associatedtype C: ~Copyable   // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
  associatedtype D: ~Escapable, ~Copyable   // expected-warning 2{{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
}

// CHECK-LABEL: .test3@
// CHECK-NEXT: Generic signature: <T where T : Escapable, T : P4>
func test3<T>(_ p: T) where T: P4 {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=P4
// CHECK-NEXT: Generic signature: <Self where Self : Escapable, Self : P4>
extension P4 {}

// CHECK-LABEL: .Explicit@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self : Transferable, Self.[Explicit]Elm : Copyable, Self.[Explicit]Elm : Escapable, Self.[Explicit]Elm : Transferable>
protocol Explicit: Copyable, Escapable {
  associatedtype Elm: Copyable, Escapable
}
