// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-silgen -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-sil -sil-verify-all %s | %FileCheck %s --check-prefix=CANON

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}

@com(interface: "51000000-0000-0000-0000-000000000002")
protocol IClassItem: IItem, AnyObject {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}6borrow
// CHECK: moveonlywrapper_to_copyable [guaranteed]
// CHECK-NEXT: [[POINTER:%.*]] = unchecked_trivial_bit_cast {{%.*}} : $any IItem to $Builtin.RawPointer
// CHECK: return [[POINTER]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}6borrow
// CANON-NOT: copy_value
// CANON-NOT: destroy_value
// CANON: return
func borrow(_ value: borrowing any IItem) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}6retain
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $any IItem
// CHECK-NEXT: [[OWNED:%.*]] = copy_value [[REFERENCE]]
// CHECK: return [[OWNED]]
func retain(_ pointer: Builtin.RawPointer) -> any IItem {
  Builtin.bridgeFromRawPointer(pointer)
}

// Class-bound COM existentials use the same interface representation.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11borrowClass
// CHECK: moveonlywrapper_to_copyable [guaranteed]
// CHECK-NEXT: [[POINTER:%.*]] = unchecked_trivial_bit_cast {{%.*}} : $any IClassItem to $Builtin.RawPointer
// CHECK: return [[POINTER]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}11borrowClass
// CANON-NOT: copy_value
// CANON-NOT: destroy_value
// CANON: return
func borrowClass(_ value: borrowing any IClassItem) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11retainClass
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $any IClassItem
// CHECK-NEXT: [[OWNED:%.*]] = copy_value [[REFERENCE]]
// CHECK: return [[OWNED]]
func retainClass(_ pointer: Builtin.RawPointer) -> any IClassItem {
  Builtin.bridgeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4pass
// CHECK: [[POINTER:%.*]] = unchecked_trivial_bit_cast %0 : $any IItem to $Builtin.RawPointer
// CHECK: return [[POINTER]]
func pass(_ value: any IItem) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4take
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $any IItem
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $any IItem, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func take(_ pointer: Builtin.RawPointer) -> any IItem {
  Builtin.takeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}9takeClass
// CHECK: [[REFERENCE:%.*]] = unchecked_bitwise_cast %0 : $Builtin.RawPointer to $any IClassItem
// CHECK-NEXT: [[OWNED:%.*]] = unchecked_ownership_conversion [[REFERENCE]] : $any IClassItem, @unowned to @owned
// CHECK-NOT: copy_value
// CHECK: return [[OWNED]]
func takeClass(_ pointer: Builtin.RawPointer) -> any IClassItem {
  Builtin.takeFromRawPointer(pointer)
}
