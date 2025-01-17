// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCasePointerPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCasePointerPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCasePointerPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class Marker {
	let value = 1
}

enum SingleCasePointerPayloadEnum {
case only(Marker)
}

class ClassWithSingleCasePointerPayloadEnum {
  var e1: SingleCasePointerPayloadEnum?
  var e2: SingleCasePointerPayloadEnum = .only(Marker())
  var e3: SingleCasePointerPayloadEnum? = .some(.only(Marker()))
  var e4: SingleCasePointerPayloadEnum??
}

reflect(object: ClassWithSingleCasePointerPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_SingleCasePointerPayload.ClassWithSingleCasePointerPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (reference kind=strong refcounting=native))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e2 offset=24
// CHECK-64:     (reference kind=strong refcounting=native))
// CHECK-64:   (field name=e3 offset=32
// CHECK-64:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (reference kind=strong refcounting=native))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e4 offset=40
// CHECK-64:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-2]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (reference kind=strong refcounting=native))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_SingleCasePointerPayload.ClassWithSingleCasePointerPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=24 alignment=4 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (reference kind=strong refcounting=native))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e2 offset=12
// CHECK-32:     (reference kind=strong refcounting=native))
// CHECK-32:   (field name=e3 offset=16
// CHECK-32:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (reference kind=strong refcounting=native))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e4 offset=20
// CHECK-32:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4094 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (reference kind=strong refcounting=native))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1))))

reflect(enum: SingleCasePointerPayloadEnum.only(Marker()))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_SingleCasePointerPayload.SingleCasePointerPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)

// CHECK-64: Enum value:
// CHECK-64: (reference kind=strong refcounting=native)

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK-32: Type reference:
// CHECK-32: (enum reflect_Enum_SingleCasePointerPayload.SingleCasePointerPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)

// CHECK-32: Enum value:
// CHECK-32: (reference kind=strong refcounting=native)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
