// RUN: %target-swift-frontend -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes

// Regression test: emitting metadata for a struct containing a
// `Builtin.Borrow<E>` field, where `E` is a single-payload enum whose payload
// has extra inhabitants, was tripping an assertion because the Borrow's type was being
// sent down instead of the referent's type.

import Builtin

// Tests verify that the `getEnumTagSinglePayload` is emitted in the VWT

// CHECK: define {{.*}} @"$s{{.*}}12BorrowOptAnyVwet"
struct BorrowOptAny: ~Escapable {
  let b: Builtin.Borrow<AnyObject?>
}

final class MyClass {}
// CHECK: define {{.*}} @"$s{{.*}}14BorrowOptKlassVwet"
struct BorrowOptKlass: ~Escapable {
  let b: Builtin.Borrow<MyClass?>
}

// Bool has many extra inhabitants (0x02..0xFF); same trigger path as classes.
// CHECK: define {{.*}} @"$s{{.*}}13BorrowOptBoolVwet"
struct BorrowOptBool: ~Escapable {
  let b: Builtin.Borrow<Bool?>
}

protocol P: AnyObject {}
// CHECK: define {{.*}} @"$s{{.*}}10BorrowOptPVwet"
struct BorrowOptP: ~Escapable {
  let b: Builtin.Borrow<(any P)?>
}

// Single-payload enum that isn't Optional, to confirm the trigger isn't
// specific to `Optional`.
enum E { case some(AnyObject); case none }
// CHECK: define {{.*}} @"$s{{.*}}10BorrowEnumVwet"
struct BorrowEnum: ~Escapable {
  let b: Builtin.Borrow<E>
}
