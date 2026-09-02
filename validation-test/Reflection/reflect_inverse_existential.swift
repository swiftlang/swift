// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_inverse_existential
// RUN: %target-codesign %t/reflect_inverse_existential

// RUN: %target-run %target-swift-reflection-test %t/reflect_inverse_existential | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: ~Copyable inverse requirement dropped from constrained existential and SILBox
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/TypeRefBuilder.h:1304-1312 — createConstrainedExistentialType has "// FIXME: Handle inverse requirements." and silently drops the InverseRequirements array. include/swift/RemoteInspection/TypeRefBuilder.h:1405-1413 — createSILBoxTypeWithLayout has the same FIXME and discards InverseRequirements. stdlib/public/RemoteInspection/TypeRef.cpp:283-290 — visitConstrainedExistentialTypeRef only iterates getRequirements() and has no path for inverse-protocol markers, so even if the data were stored it could not be dumped.
// Runtime-Reference: include/swift/ABI/MetadataValues.h InvertibleProtocolKind and the extended-existential shape caching in stdlib/public/runtime/Metadata.cpp preserve inverse-protocol bits on the in-process metadata, so the runtime can answer queries like "is the dynamic type Copyable?" correctly.
// Description:
//   When a value's static type is `any P & ~Copyable`, the constrained
//   existential's TypeRef in RemoteInspection is constructed without any
//   record of the inverse Copyable requirement; both the constrained
//   existential builder and the SIL box builder ignore the
//   InverseRequirements parameter. Even if they propagated it, the dump
//   visitor has no branch that prints inverse markers. The runtime, in
//   contrast, tracks InvertibleProtocolKind on the extended-existential
//   shape. Closing the gap means storing the inverse requirements on the
//   ConstrainedExistentialTypeRef / SILBoxTypeWithLayoutTypeRef and
//   dumping a marker like `(inverse Copyable)` next to the existential.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

protocol P: ~Copyable {
  func ping()
}

struct S: P, ~Copyable {
  func ping() {}
}

class InverseHolder {
  var fn: (consuming any P & ~Copyable) -> Void = { _ in }
}

reflect(object: InverseHolder())

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}InverseHolder
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=fn
// CHECK: (function
// CHECK: (inverse {{.*}}Copyable

doneReflecting()

// CHECK: Done.
