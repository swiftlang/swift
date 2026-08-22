// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -O -emit-ir %s -o - | %FileCheck %s -implicit-check-not swift_retain -implicit-check-not swift_release

import COM

@com(interface: "42000000-0000-0000-0000-000000000001")
public protocol IForeign {
}

// Adopting a "+1" interface pointer changes its ownership representation but
// does not change the pointer or its reference count.

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}5adopt
// CHECK-SAME:  (ptr [[POINTER:%.*]])
// CHECK:         ret ptr [[POINTER]]
@inline(never)
public func adopt(_ pointer: UnsafeMutableRawPointer) -> any IForeign {
  ManagedObject<IForeign>.takeRetainedValue(pointer)
}

// The typed optional overload preserves nil and erases only the pointee type.

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}13adoptOptional
// CHECK-SAME:  (ptr [[POINTER:%.*]])
// CHECK:         ret ptr [[POINTER]]
@inline(never)
public func adoptOptional(_ pointer: UnsafeMutablePointer<CChar>?)
    -> (any IForeign)? {
  ManagedObject<IForeign>.takeRetainedValue(pointer)
}

// Borrowing an existing reference exposes the same ABI pointer at "+0".

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}4pass
// CHECK-SAME:  (ptr [[INTERFACE:%.*]])
// CHECK:         ret ptr [[INTERFACE]]
@inline(never)
public func pass(_ interface: borrowing any IForeign)
    -> UnsafeMutableRawPointer {
  ManagedObject<IForeign>.passUnretained(interface)
}
