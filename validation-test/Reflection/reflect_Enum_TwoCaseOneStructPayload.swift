// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_TwoCaseOneStructPayload
// RUN: %target-codesign %t/reflect_Enum_TwoCaseOneStructPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_TwoCaseOneStructPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-ALL

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

// Note: struct Marker has no extra inhabitants, so the enum will use a separate tag
struct Marker {
	let value = 1
}

enum TwoCaseOneStructPayloadEnum {
case valid(Marker)
case invalid
}

class ClassWithTwoCaseOneStructPayloadEnum {
  var e1: TwoCaseOneStructPayloadEnum?
  var e2: TwoCaseOneStructPayloadEnum = .valid(Marker())
  var e3: TwoCaseOneStructPayloadEnum = .invalid
  var e4: TwoCaseOneStructPayloadEnum? = .valid(Marker())
  var e5: TwoCaseOneStructPayloadEnum? = .invalid
	var e6: TwoCaseOneStructPayloadEnum??
}

reflect(object: ClassWithTwoCaseOneStructPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_TwoCaseOneStructPayload.ClassWithTwoCaseOneStructPayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=107 alignment=8 stride=112 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=valid index=0 offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:           (case name=invalid index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e2 offset=32
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=valid index=0 offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=value offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=_value offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:       (case name=invalid index=1)))
// CHECK-64:   (field name=e3 offset=48
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=valid index=0 offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=value offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=_value offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:       (case name=invalid index=1)))
// CHECK-64:   (field name=e4 offset=64
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=valid index=0 offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:           (case name=invalid index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e5 offset=80
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=valid index=0 offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:           (case name=invalid index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e6 offset=96
// CHECK-64:     (single_payload_enum size=11 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (case name=valid index=0 offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=value offset=0
// CHECK-64:                     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                       (field name=_value offset=0
// CHECK-64:                         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:               (case name=invalid index=1)))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_TwoCaseOneStructPayload.ClassWithTwoCaseOneStructPayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=55 alignment=4 stride=56 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=valid index=0 offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:           (case name=invalid index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e2 offset=16
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=valid index=0 offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=value offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=_value offset=0
// CHECK-32:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:       (case name=invalid index=1)))
// CHECK-32:   (field name=e3 offset=24
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=valid index=0 offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=value offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=_value offset=0
// CHECK-32:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:       (case name=invalid index=1)))
// CHECK-32:   (field name=e4 offset=32
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=valid index=0 offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:           (case name=invalid index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e5 offset=40
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=valid index=0 offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:           (case name=invalid index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e6 offset=48
// CHECK-32:     (single_payload_enum size=7 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (case name=valid index=0 offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=value offset=0
// CHECK-32:                     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                       (field name=_value offset=0
// CHECK-32:                         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:               (case name=invalid index=1)))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1))))

reflect(enum: TwoCaseOneStructPayloadEnum.valid(Marker()))

// CHECK-ALL: Reflecting an enum.
// CHECK-ALL-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-ALL-NEXT: Type reference:
// CHECK-ALL-NEXT: (enum reflect_Enum_TwoCaseOneStructPayload.TwoCaseOneStructPayloadEnum)

// CHECK-ALL: Type info:
// CHECK-64-NEXT: (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (case name=valid index=0 offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (case name=invalid index=1))

// CHECK-32-NEXT: (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (case name=valid index=0 offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (case name=invalid index=1))

// CHECK-ALL: Enum value:
// CHECK-ALL-NEXT: (enum_value name=valid index=0
// CHECK-ALL-NEXT:   (struct reflect_Enum_TwoCaseOneStructPayload.Marker)
// CHECK-ALL-NEXT: )

reflect(enum: TwoCaseOneStructPayloadEnum.invalid)

// CHECK-ALL: Reflecting an enum.
// CHECK-ALL-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-ALL-NEXT: Type reference:
// CHECK-ALL-NEXT: (enum reflect_Enum_TwoCaseOneStructPayload.TwoCaseOneStructPayloadEnum)

// CHECK-ALL: Type info:
// CHECK-64-NEXT: (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (case name=valid index=0 offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (case name=invalid index=1))

// CHECK-32-NEXT: (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (case name=valid index=0 offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (case name=invalid index=1))

// CHECK-ALL: Enum value:
// CHECK-ALL-NEXT: (enum_value name=invalid index=1)

doneReflecting()

// CHECK-ALL: Done.
