// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCaseVoidPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCaseVoidPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCaseVoidPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct Marker {
	let value = 1
}

enum Trivial {
case only
}

enum SingleCaseVoidPayloadEnum {
case `default`(Trivial)
}

class ClassWithSingleCaseVoidPayloadEnum {
  var e1: SingleCaseVoidPayloadEnum?
  var e2: SingleCaseVoidPayloadEnum = .`default`(Trivial.only)
  var e3: SingleCaseVoidPayloadEnum? = .`default`(Trivial.only)
  var e4: SingleCaseVoidPayloadEnum??
  let marker = Marker()
}

reflect(object: ClassWithSingleCaseVoidPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64-NEXT: Type reference:
// CHECK-64-NEXT: (class reflect_Enum_SingleCaseVoidPayload.ClassWithSingleCaseVoidPayloadEnum)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=e1 offset=16
// CHECK-64-NEXT:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (case name=default index=0 offset=0
// CHECK-64-NEXT:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (case name=only index=0)))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=e2 offset=17
// CHECK-64-NEXT:     (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (case name=default index=0 offset=0
// CHECK-64-NEXT:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (case name=only index=0)))))
// CHECK-64-NEXT:   (field name=e3 offset=17
// CHECK-64-NEXT:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (case name=default index=0
// CHECK-64-NEXT:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (case name=only index=0)))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=e4 offset=18
// CHECK-64-NEXT:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (case name=some index=0 offset=0
// CHECK-64-NEXT:             (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (case name=default index=0
// CHECK-64-NEXT:                 (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (case name=only index=0)))))
// CHECK-64-NEXT:           (case name=none index=1)))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=marker offset=24
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

// CHECK-32: Reflecting an object.
// CHECK-32-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32-NEXT: Type reference:
// CHECK-32-NEXT: (class reflect_Enum_SingleCaseVoidPayload.ClassWithSingleCaseVoidPayloadEnum)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=e1 offset=8
// CHECK-32-NEXT:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (case name=default index=0
// CHECK-32-NEXT:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (case name=only index=0)))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=e2 offset=9
// CHECK-32-NEXT:     (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (case name=default index=0
// CHECK-32-NEXT:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (case name=only index=0)))))
// CHECK-32-NEXT:   (field name=e3 offset=9
// CHECK-32-NEXT:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (case name=default index=0
// CHECK-32-NEXT:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (case name=only index=0)))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=e4 offset=10
// CHECK-32-NEXT:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (case name=some index=0 offset=0
// CHECK-32-NEXT:             (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (case name=default index=0
// CHECK-32-NEXT:                 (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (case name=only index=0)))))
// CHECK-32-NEXT:           (case name=none index=1)))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=marker offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))

reflect(enum: SingleCaseVoidPayloadEnum.`default`(Trivial.only))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_SingleCaseVoidPayload.SingleCaseVoidPayloadEnum)

// CHECK: Type info:
// CHECK-NEXT: (single_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:   (case name=default index=0
// CHECK-NEXT:     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:       (case name=only index=0))))


// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=default index=0
// CHECK-NEXT:   (enum reflect_Enum_SingleCaseVoidPayload.Trivial)
// CHECK-NEXT: )

doneReflecting()

// CHECK: Done.
