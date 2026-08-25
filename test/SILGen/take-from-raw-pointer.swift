// RUN: %target-swift-frontend -swift-version 5 \
// RUN:   -enable-experimental-feature BuiltinModule -emit-silgen \
// RUN:   -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

final class Object {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4take
// CHECK:         [[REFERENCE:%.*]] = raw_pointer_to_ref %0 : $Builtin.RawPointer to $Object
// CHECK-NEXT:    [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $Object, @unowned to @owned
// CHECK-NOT:     copy_value
// CHECK:         return [[OWNED]]
func take(_ pointer: Builtin.RawPointer) -> Object {
  Builtin.takeFromRawPointer(pointer)
}

// A generic result remains address-only at its abstraction boundary. Taking
// the pointer initializes the indirect result without copying the reference.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11genericTake
// CHECK:         [[POINTER:%.*]] = alloc_stack $Builtin.RawPointer
// CHECK:         store %1 to [trivial] [[POINTER]]
// CHECK:         [[VALUE:%.*]] = unchecked_addr_cast [[POINTER]]{{.*}}to $*T
// CHECK:         copy_addr [take] [[VALUE]] to [init] %0 : $*T
func genericTake<T>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.takeFromRawPointer(pointer)
}

// Bridging a "+0" pointer copies the address-only reference into the result.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}13genericBridge
// CHECK:         [[POINTER:%.*]] = alloc_stack $Builtin.RawPointer
// CHECK:         store %1 to [trivial] [[POINTER]]
// CHECK:         [[VALUE:%.*]] = unchecked_addr_cast [[POINTER]]{{.*}}to $*T
// CHECK:         copy_addr [[VALUE]] to [init] %0 : $*T
func genericBridge<T>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.bridgeFromRawPointer(pointer)
}

// Borrowing an address-only value exposes its pointer without copying it.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11genericPass
// CHECK-NOT:     copy_addr
// CHECK-NOT:     copy_value
// CHECK:         [[POINTER:%.*]] = unchecked_addr_cast {{%.*}}to $*Builtin.RawPointer
// CHECK:         [[VALUE:%.*]] = load [trivial] [[POINTER]]
// CHECK-NOT:     copy_addr
// CHECK-NOT:     copy_value
// CHECK:         return [[VALUE]]
func genericPass<T>(_ value: borrowing T) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}
