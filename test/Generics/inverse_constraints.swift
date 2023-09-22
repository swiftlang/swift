// RUN: %target-swift-frontend -enable-experimental-feature NoncopyableGenerics -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// REQUIRES: asserts

// CHECK-LABEL: (file).genericFn@
// CHECK: Generic signature: <T where T : Copyable>
func genericFn<T>(_ t: T) {}

// CHECK-LABEL: .withInverse@
// CHECK: Generic signature: <T>
func withInverse<T: ~Copyable>(_ t: T) {}

// CHECK-LABEL: .S1@
// CHECK: Generic signature: <T where T : Copyable>
struct S1<T> {}

// CHECK-LABEL: .S1_I@
// CHECK: Generic signature: <T>
struct S1_I<T: ~Copyable> {}

// CHECK-LABEL: .C1@
// CHECK: Generic signature: <T, U where T : Copyable, U : Copyable>
class C1<T, U> {}

// CHECK-LABEL: .C1_IC@
// CHECK: Generic signature: <T, U where U : Copyable>
class C1_IC<T: ~Copyable, U> {}

// CHECK-LABEL: .C1_CI@
// CHECK: Generic signature: <T, U where T : Copyable>
class C1_CI<T, U: ~Copyable> {}

// CHECK-LABEL: .C1_II@
// CHECK: Generic signature: <T, U>
class C1_II<T: ~Copyable, U: ~Copyable> {}

// CHECK-LABEL: .P1@
// CHECK: Requirement signature: <Self where Self : Copyable>
protocol P1 {}

// CHECK-LABEL: .P2@
// CHECK: <Self where Self : Copyable, Self.[P2]A : Copyable>
protocol P2 { associatedtype A }

// CHECK-LABEL: .P2_IC@
// CHECK: <Self where Self.[P2_IC]A : Copyable>
protocol P2_IC: ~Copyable { associatedtype A }

// CHECK-LABEL: .P2_CI@
// CHECK: Requirement signature: <Self where Self : Copyable>
protocol P2_CI { associatedtype A: ~Copyable }

// CHECK-LABEL: .P2_II@
// CHECK: Requirement signature: <Self>
protocol P2_II: ~Copyable { associatedtype A: ~Copyable }
