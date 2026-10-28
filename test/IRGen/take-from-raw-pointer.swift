// RUN: %target-swift-frontend -enable-builtin-module -emit-ir -sil-verify-all -disable-llvm-optzns %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-builtin-module -emit-ir -sil-verify-all -O %s | %FileCheck %s

import Builtin

public class Object {}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}4take
// CHECK-SAME: (ptr{{[^%]*}} [[POINTER:%[^,)]+]])
// CHECK-NOT: load ptr
// CHECK-NOT: call{{.*}} @swift_
// CHECK: ret ptr [[POINTER]]
public func take(_ pointer: UnsafeRawPointer) -> Object {
  Builtin.takeFromRawPointer(pointer._rawValue)
}
