// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_gap_value_generics
// RUN: %target-codesign %t/reflect_gap_value_generics

// RUN: %target-run %target-swift-reflection-test %t/reflect_gap_value_generics | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: integer (value) generic argument cannot be lowered
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/TypeRefBuilder.h:953-959 — createIntegerType / createNegativeIntegerType construct an IntegerTypeRef. include/swift/RemoteInspection/TypeRef.h:1211-1227 — IntegerTypeRef carries the literal value. stdlib/public/RemoteInspection/TypeLowering.cpp:2761-2764 — visitIntegerTypeRef sets "can't lower integer TypeRef" and returns nullptr, so any bound generic that uses an integer argument fails to lower.
// Runtime-Reference: include/swift/ABI/MetadataValues.h GenericPackKind / GenericValueArgument and the value-generic handling threaded through the in-process generic-argument walk allow the runtime to project a bound-generic with a value argument and report its layout.
// Description:
//   Types parameterized by integer generics (e.g. `<let N: Int>`) reach
//   RemoteInspection with an IntegerTypeRef argument that the lowering
//   visitor refuses outright. Reflecting an existential boxing
//   `IntegerHolder<3>` therefore prints
//   `swift_reflection_projectExistential failed.` rather than producing a
//   TypeRef and TypeInfo for the boxed value. The in-process Mirror handles
//   the same value-generic argument through the runtime's generic-argument
//   walk. Closing the gap means giving visitIntegerTypeRef (and any
//   downstream substitution that re-enters lowering) a path that produces
//   an empty TypeInfo or otherwise lets the parent BoundGeneric lower.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

struct IntegerHolder<let N: Int> { let value: Int }

reflect(any: IntegerHolder<3>(value: 7))

// CHECK: Reflecting an existential.
// CHECK-NOT: swift_reflection_projectExistential failed.
// CHECK: Type reference:
// CHECK: (bound_generic_struct {{.*}}IntegerHolder
// CHECK: Type info:
// CHECK: (struct
// CHECK: (field name=value

doneReflecting()

// CHECK: Done.
