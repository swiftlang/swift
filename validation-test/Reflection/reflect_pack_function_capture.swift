// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_pack_function_capture
// RUN: %target-codesign %t/reflect_pack_function_capture

// RUN: %target-run %target-swift-reflection-test %t/reflect_pack_function_capture | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: pack TypeRefs in closure captures cannot be lowered
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/TypeRefBuilder.h:1179-1183 — createSILPackType returns nullptr ("FIXME: Remote mirrors support for variadic generics"). stdlib/public/RemoteInspection/TypeLowering.cpp:2558-2566 — visitPackTypeRef and visitPackExpansionTypeRef set "cannot have pack type here" / "cannot have pack expansion type here" errors and return nullptr.
// Runtime-Reference: stdlib/public/runtime/Metadata.cpp:1191-1212 — swift_allocateMetadataPack and swift_allocateWitnessTablePack manage in-process pack metadata. include/swift/ABI/Metadata.h:2130-2354 — TargetMetadataPackMetadata and TargetGenericPackShapeMetadata describe the runtime layout consumed by the in-process runtime.
// Description:
//   When a closure captures a value whose type contains a pack expansion
//   (`(repeat each T)`), SILGen emits a SIL pack capture in the closure's
//   capture descriptor. RemoteInspection cannot reconstruct that capture:
//   createSILPackType returns nullptr, and even when a plain PackTypeRef
//   reaches lowering, visitPackTypeRef and visitPackExpansionTypeRef bail
//   with "cannot have pack type here". The result is a closure whose
//   captured environment is missing or incomplete in the dump. The
//   in-process runtime walks pack metadata routinely via the
//   swift_allocate*Pack helpers, so closing the gap means teaching
//   RemoteInspection how to materialize PackTypeRefs and lower them as
//   tuple-shaped fields.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

@available(macOS 14.0, *)
class PackBox<each T> {
  var fn: () -> Void = {}
  init(_ values: repeat each T) {
    let captured = (repeat each values)
    self.fn = { _ = captured }
  }
}

if #available(macOS 14.0, *) {
  let box = PackBox<Int, String>(42, "hello")
  reflect(function: box.fn)
}

// CHECK: Reflecting an object.
// CHECK-NOT: cannot have pack type here
// CHECK-NOT: cannot have pack expansion type here
// CHECK: Type reference:
// CHECK: (builtin Builtin.NativeObject)
// CHECK: Type info:
// CHECK: (closure_context
// CHECK: (tuple
// CHECK: (struct Swift.Int
// CHECK: (struct Swift.String

doneReflecting()

// CHECK: Done.
