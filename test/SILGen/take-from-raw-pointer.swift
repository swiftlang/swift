// RUN: %target-swift-frontend -swift-version 5 \
// RUN:   -enable-experimental-feature BuiltinModule -emit-silgen \
// RUN:   -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

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
