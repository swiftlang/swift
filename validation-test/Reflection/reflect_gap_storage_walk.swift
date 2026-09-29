// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_gap_storage_walk
// RUN: %target-codesign %t/reflect_gap_storage_walk

// RUN: %target-run %target-swift-reflection-test %t/reflect_gap_storage_walk | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: weak/unowned/unmanaged class fields stop the remote walk at the reference
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/TypeRef.h:1098-1112 — WeakStorageTypeRef, UnownedStorageTypeRef, and UnmanagedStorageTypeRef are all generated from the shared REF_STORAGE macro and inherit from ReferenceStorageTypeRef. stdlib/public/RemoteInspection/TypeLowering.cpp:2731-2736 — the visit*StorageTypeRef visitors all delegate to visitAnyStorageTypeRef, which lowers the pointee and wraps it in a ReferenceTypeInfo. stdlib/public/SwiftRemoteMirror/SwiftRemoteMirror.cpp:602-628 — convertChild only handles ArrayTypeInfo, EnumTypeInfo, and RecordTypeInfo; ReferenceTypeInfo falls into the trailing assert/return-empty branch, so the walk terminates at the reference instead of stepping into the pointee.
// Runtime-Reference: stdlib/public/runtime/HeapObject.cpp / RuntimeHeaders for swift_weakLoadStrong, swift_unownedRetainStrong, and the Unmanaged-as-raw-pointer convention let the in-process Mirror reload the strong reference and recurse into the pointee for all three qualifiers.
// Description:
//   Reflecting a class instance with a `weak`, `unowned`, or
//   `unowned(unsafe)` (a.k.a. unmanaged) field today reports
//   `(reference kind={weak,unowned,unmanaged} refcounting=...)` for the
//   field and stops — the walk does not recurse into the referenced class.
//   In-process Mirror does recurse. The three qualifiers go through one
//   shared TypeRef class hierarchy and one shared lowering visitor, and
//   the walk stops in the same place: convertChild does not know what to
//   do with a ReferenceTypeInfo, so it bails. A single fix in convertChild
//   that extracts the pointee TypeRef from the ReferenceTypeInfo and
//   recurses should close the gap for all three qualifiers, which is why
//   one consolidated XFAIL test covers them. (`gaps_dynamic.md` groups
//   `reflect_WeakStorage`, `reflect_UnownedStorage`, and
//   `reflect_UnmanagedStorage` into the same `field-count at /field[t]/`
//   cluster.) When the gap closes this test will XPASS — remove the
//   expected-failure marker and verify the desired output below.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

class Inner {
  var n: Int = 5
}

class WeakHolder {
  weak var t: Inner?
}

class UnownedHolder {
  unowned let t: Inner
  init(_ t: Inner) { self.t = t }
}

class UnmanagedHolder {
  unowned(unsafe) let t: Inner
  init(_ t: Inner) { self.t = t }
}

let weakInner = Inner()
let weakHolder = WeakHolder()
weakHolder.t = weakInner
reflect(object: weakHolder)

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}WeakHolder)
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=t offset=
// CHECK-NOT: (reference kind=weak
// CHECK: (class_instance
// CHECK: (field name=n

let unownedInner = Inner()
let unownedHolder = UnownedHolder(unownedInner)
reflect(object: unownedHolder)

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}UnownedHolder)
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=t offset=
// CHECK-NOT: (reference kind=unowned
// CHECK: (class_instance
// CHECK: (field name=n

let unmanagedInner = Inner()
let unmanagedHolder = UnmanagedHolder(unmanagedInner)
reflect(object: unmanagedHolder)

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}UnmanagedHolder)
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=t offset=
// CHECK-NOT: (reference kind=unmanaged
// CHECK: (class_instance
// CHECK: (field name=n

_ = weakInner
_ = unownedInner
_ = unmanagedInner

doneReflecting()

// CHECK: Done.
