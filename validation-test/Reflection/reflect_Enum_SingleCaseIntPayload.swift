// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCaseIntPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCaseIntPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCaseIntPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

enum SingleCaseIntPayloadEnum {
case only(Int)
}

class ClassWithSingleCaseIntPayloadEnum {
  var e1: SingleCaseIntPayloadEnum?
  var e2: SingleCaseIntPayloadEnum = .only(1)
  var e3: SingleCaseIntPayloadEnum? = .some(.only(2))
  var e4: SingleCaseIntPayloadEnum??
}

reflect(object: ClassWithSingleCaseIntPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_SingleCaseIntPayload.ClassWithSingleCaseIntPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=66 alignment=8 stride=72 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=_value offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e2 offset=32
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64:   (field name=e3 offset=40
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=_value offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e4 offset=56
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=_value offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_SingleCaseIntPayload.ClassWithSingleCaseIntPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=34 alignment=4 stride=36 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=_value offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e2 offset=16
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32:   (field name=e3 offset=20
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=_value offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e4 offset=28
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=_value offset=0
// CHECK-32:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32:           (case name=none index=1))
// CHECK-32:       (case name=none index=1))))

reflect(enum: SingleCaseIntPayloadEnum.only(77))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_SingleCaseIntPayload.SingleCaseIntPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=_value offset=0
// CHECK-64:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))

// CHECK-64: Enum value:
// CHECK-64: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=_value offset=0
// CHECK-64:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
