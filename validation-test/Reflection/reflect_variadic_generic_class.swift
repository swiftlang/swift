// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_variadic_generic_class
// RUN: %target-codesign %t/reflect_variadic_generic_class

// RUN: %target-run %target-swift-reflection-test %t/reflect_variadic_generic_class | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: variadic generic class instance with a pack-typed stored field
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/TypeRefBuilder.h:1358-1362 — createGenericTypeParameterType carries a "// FIXME: variadic generics" comment and does not distinguish pack-shaped parameters. stdlib/public/RemoteInspection/TypeLowering.cpp:2558-2566 — even when PackTypeRef survives construction, visitPackTypeRef / visitPackExpansionTypeRef refuse to lower it.
// Runtime-Reference: stdlib/public/runtime/Metadata.cpp:1191-1212 — swift_allocateMetadataPack and the MetadataKind::Pack handling in include/swift/ABI/MetadataValues.h let the in-process runtime walk packs that appear as generic arguments and as field types.
// Description:
//   Companion to the pack-function-capture gap: even when the closure
//   wrinkle is removed, a plain stored field whose type is a pack tuple
//   `(repeat each T)` can't be lowered. Constructing the BoundGenericClass
//   TypeRef succeeds and PackTypeRef::dump prints `(pack ...)` correctly,
//   but field substitution introduces a residual pack expansion that the
//   TypeLowering visitor rejects. Closing the gap means lowering pack
//   tuple fields to a tuple-shaped record matching the substituted shape.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

@available(macOS 14.0, *)
class VariadicBox<each T> {
  let values: (repeat each T)
  init(_ values: repeat each T) {
    self.values = (repeat each values)
  }
}

if #available(macOS 14.0, *) {
  reflect(object: VariadicBox<Int, String>(42, "hello"))
}

// CHECK: Reflecting an object.
// CHECK-NOT: cannot have pack type here
// CHECK-NOT: cannot have pack expansion type here
// CHECK: Type reference:
// CHECK: (bound_generic_class {{.*}}VariadicBox
// CHECK: (pack
// CHECK: (struct Swift.Int)
// CHECK: (struct Swift.String)
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=values
// CHECK: (tuple
// CHECK: (struct Swift.Int
// CHECK: (struct Swift.String

doneReflecting()

// CHECK: Done.
