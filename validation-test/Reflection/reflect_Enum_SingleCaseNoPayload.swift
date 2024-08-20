// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCaseNoPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCaseNoPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCaseNoPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct Marker {
	let value = 1
}

enum SingleCaseNoPayloadEnum {
case `default`
}

class ClassWithSingleCaseNoPayloadEnum {
  var e1: SingleCaseNoPayloadEnum?
  var e2: SingleCaseNoPayloadEnum = .`default`
  var e3: SingleCaseNoPayloadEnum? = .`default`
  var e4: SingleCaseNoPayloadEnum??
  let marker = Marker()
	
}

reflect(object: ClassWithSingleCaseNoPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_SingleCaseNoPayload.ClassWithSingleCaseNoPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=default index=0)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e2 offset=17
// CHECK-64:     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=default index=0)))
// CHECK-64:   (field name=e3 offset=17
// CHECK-64:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=default index=0)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e4 offset=18
// CHECK-64:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (case name=default index=0)))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=marker offset=24
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=value offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=_value offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_SingleCaseNoPayload.ClassWithSingleCaseNoPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=default index=0)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e2 offset=9
// CHECK-32:     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=default index=0)))
// CHECK-32:   (field name=e3 offset=9
// CHECK-32:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=default index=0)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e4 offset=10
// CHECK-32:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (case name=default index=0)))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=marker offset=12
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=value offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=_value offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))

reflect(enum: SingleCaseNoPayloadEnum.`default`)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_SingleCaseNoPayload.SingleCaseNoPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (case name=default index=0))

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=default index=0)

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (enum reflect_Enum_SingleCaseNoPayload.SingleCaseNoPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (case name=default index=0))

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=default index=0)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
