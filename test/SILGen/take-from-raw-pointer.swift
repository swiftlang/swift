// RUN: %target-swift-emit-silgen -enable-builtin-module -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s

import Builtin

class Object {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4take
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $Object
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $Object, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func take(_ pointer: Builtin.RawPointer) -> Object {
  Builtin.takeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11takeGeneric
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $T
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $T, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func takeGeneric<T: Object>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.takeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}10takeNative
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $Builtin.NativeObject
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $Builtin.NativeObject, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func takeNative(_ pointer: Builtin.RawPointer) -> Builtin.NativeObject {
  Builtin.takeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}13takeAnyObject
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $AnyObject
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $AnyObject, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func takeAnyObject(_ pointer: Builtin.RawPointer) -> AnyObject {
  Builtin.takeFromRawPointer(pointer)
}
