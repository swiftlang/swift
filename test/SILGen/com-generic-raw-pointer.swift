// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-silgen -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-sil -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s --check-prefix=CANON
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -enable-sil-opaque-values -I %t -emit-silgen -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s --check-prefix=OPAQUE
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -enable-sil-opaque-values -I %t -emit-sil -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s --check-prefix=CANON

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}

@com(interface: "51000000-0000-0000-0000-000000000002")
protocol IClassItem: IItem, AnyObject {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}6borrow
// CHECK: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*{{.*}} to $*Builtin.RawPointer
// CHECK-NEXT: [[POINTER:%.*]] = load [trivial] [[ADDRESS]]
// CHECK: return [[POINTER]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}6borrow
// CANON-NOT: copy_addr
// CANON-NOT: destroy_addr
// CANON: return
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}6borrow
// OPAQUE: [[VALUE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// OPAQUE-NEXT: [[POINTER:%.*]] = unchecked_trivial_bit_cast [[VALUE]] : $T to $Builtin.RawPointer
// OPAQUE: return [[POINTER]]
func borrow<T: IItem>(_ value: borrowing T) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}6retain
// CHECK: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// CHECK-NEXT: copy_addr [[ADDRESS]] to [init] %0 : $*T
// CHECK: return
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}6retain{{.*}} : $@convention(thin) <T where T : IItem>
// OPAQUE: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// OPAQUE-NEXT: [[VALUE:%.*]] = load [copy] [[ADDRESS]] : $*T
// OPAQUE: return [[VALUE]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}6retain{{.*}} : $@convention(thin) <T where T : IItem>
// CANON: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// CANON-NEXT: copy_addr [[ADDRESS]] to [init] %0 : $*T
// CANON: return
func retain<T: IItem>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.bridgeFromRawPointer(pointer)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11borrowClass
// CHECK: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*{{.*}} to $*Builtin.RawPointer
// CHECK-NEXT: [[POINTER:%.*]] = load [trivial] [[ADDRESS]]
// CHECK: return [[POINTER]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}11borrowClass
// CANON-NOT: copy_addr
// CANON-NOT: destroy_addr
// CANON: return
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}11borrowClass
// OPAQUE: [[VALUE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// OPAQUE-NEXT: [[POINTER:%.*]] = unchecked_trivial_bit_cast [[VALUE]] : $T to $Builtin.RawPointer
// OPAQUE: return [[POINTER]]
func borrowClass<T: IClassItem>(_ value: borrowing T) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11retainClass
// CHECK: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// CHECK-NEXT: copy_addr [[ADDRESS]] to [init] %0 : $*T
// CHECK: return
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}11retainClass{{.*}} : $@convention(thin) <T where T : IClassItem>
// OPAQUE: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// OPAQUE-NEXT: [[VALUE:%.*]] = load [copy] [[ADDRESS]] : $*T
// OPAQUE: return [[VALUE]]
// CANON-LABEL: sil hidden{{.*}} @$s{{.*}}11retainClass{{.*}} : $@convention(thin) <T where T : IClassItem>
// CANON: [[ADDRESS:%.*]] = unchecked_addr_cast {{%.*}} : $*Builtin.RawPointer to $*T
// CANON-NEXT: copy_addr [[ADDRESS]] to [init] %0 : $*T
// CANON: return
func retainClass<T: IClassItem>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.bridgeFromRawPointer(pointer)
}
