// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_multipayload_enum_descriptor
// RUN: %target-codesign %t/reflect_multipayload_enum_descriptor

// RUN: %target-run %target-swift-reflection-test %t/reflect_multipayload_enum_descriptor | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: FieldDescriptorKind::MultiPayloadEnum spare-bit descriptor format ignored
// Status: XFAIL
// RemoteInspection-Reference: include/swift/RemoteInspection/Records.h:152-158 — FieldDescriptorKind::MultiPayloadEnum is documented to "encode spare bits" but the comment says "FIXME: Actually implement this. For now, a descriptor with this kind just means we also have a builtin descriptor from which we get the size and alignment." stdlib/public/RemoteInspection/TypeLowering.cpp:2524-2528 — the lowering switch shares the regular Enum path for MultiPayloadEnum, with no spare-bit-aware extra inhabitant adjustment.
// Runtime-Reference: stdlib/public/runtime/EnumImpl.h MultiPayloadEnumImpl, the TargetMultiPayloadEnumDescriptor consumers in lib/IRGen/GenReflection.cpp (descriptor producer), and runtime swift_initEnumMetadataMultiPayload — together they consume the spare-bit map encoded in the multi-payload-enum descriptor when computing the in-process metadata's extraInhabitantCount.
// Description:
//   IRGen emits a TargetMultiPayloadEnumDescriptor that encodes the enum's
//   spare-bit map. The in-process runtime uses it to derive
//   extraInhabitantCount. RemoteInspection's TypeLowering ignores the
//   spare-bit data and recomputes the count from the payload TypeInfos
//   alone. Reflecting an enum holder whose enum has pointer-typed payloads
//   (which carry spare bits in their high bits) and a payload tag that
//   would naturally be packed into those spare bits should expose the
//   divergence. If a tighter probe is needed because num_extra_inhabitants
//   coincides between the two paths for this shape, swap in a different
//   multi-payload-enum probe before landing.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

class A {}
class B {}

enum MPE {
  case payloadA(A)
  case payloadB(B)
  case stamp1
  case stamp2
}

class MPEHolder {
  var e: MPE = .payloadA(A())
}

reflect(object: MPEHolder())

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}MPEHolder
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=e
// 4-or-more-digit num_extra_inhabitants — RemoteInspection's spare-bit-blind
// path returns a 3-digit count today (e.g. 125), while the runtime, which
// honors the descriptor's spare-bit map, reports a much larger count.
// CHECK: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants={{[0-9][0-9][0-9][0-9][0-9]*}}

doneReflecting()

// CHECK: Done.
