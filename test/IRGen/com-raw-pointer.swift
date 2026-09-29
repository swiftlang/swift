// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-ir -sil-verify-all -disable-llvm-optzns %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-ir -sil-verify-all -O %s | %FileCheck %s

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
public protocol IItem {}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6borrow
// CHECK-SAME: (ptr{{[^%]*}} [[VALUE:%[^,)]+]])
// CHECK-NOT: load ptr
// CHECK-NOT: call{{.*}} @swift_
// CHECK: ret ptr [[VALUE]]
public func borrow(_ value: borrowing any IItem) -> UnsafeRawPointer {
  UnsafeRawPointer(Builtin.bridgeToRawPointer(value))
}

// Copying uses the interface's AddRef entry, never Swift reference counting.
// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6retain
// CHECK-SAME: (ptr{{[^%]*}} [[POINTER:%[^,)]+]])
// CHECK-NOT: swift_retain
// CHECK: [[VTABLE:%.*]] = load ptr, ptr [[POINTER]]
// CHECK: getelementptr inbounds{{.*}} [[VTABLE]]
// CHECK: [[ADDREF:%.*]] = load ptr
// CHECK: call{{.*}} i32 [[ADDREF]](ptr{{( nonnull)?}} [[POINTER]])
// CHECK-NOT: swift_release
// CHECK: ret ptr [[POINTER]]
public func retain(_ pointer: UnsafeRawPointer) -> any IItem {
  Builtin.bridgeFromRawPointer(pointer._rawValue)
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}4take
// CHECK-SAME: (ptr{{[^%]*}} [[POINTER:%[^,)]+]])
// CHECK-NOT: load ptr
// CHECK-NOT: call{{.*}} @swift_
// CHECK: ret ptr [[POINTER]]
public func take(_ pointer: UnsafeRawPointer) -> any IItem {
  Builtin.takeFromRawPointer(pointer._rawValue)
}
