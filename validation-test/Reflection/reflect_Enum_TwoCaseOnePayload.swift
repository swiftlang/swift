// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_TwoCaseOnePayload
// RUN: %target-codesign %t/reflect_Enum_TwoCaseOnePayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_TwoCaseOnePayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

struct Marker {
	let value = 1
}

enum TwoCaseOnePayloadEnum {
case valid(Marker)
case invalid
}

class ClassWithTwoCaseOnePayloadEnum {
  var e1: TwoCaseOnePayloadEnum?
  var e2: TwoCaseOnePayloadEnum = .valid(Marker())
  var e3: TwoCaseOnePayloadEnum = .invalid
  var e4: TwoCaseOnePayloadEnum? = .valid(Marker())
  var e5: TwoCaseOnePayloadEnum? = .invalid
	var e6: TwoCaseOnePayloadEnum??
}

reflect(object: ClassWithTwoCaseOnePayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_TwoCaseOnePayload.ClassWithTwoCaseOnePayloadEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=107 alignment=8 stride=112 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=some offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=valid offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64:   (field name=e2 offset=32
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=valid offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=value offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=_value offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK-64:   (field name=e3 offset=48
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=valid offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=value offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=_value offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK-64:   (field name=e4 offset=64
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=some offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=valid offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64:   (field name=e5 offset=80
// CHECK-64:     (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=some offset=0
// CHECK-64:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=valid offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=value offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64:   (field name=e6 offset=96
// CHECK-64:     (single_payload_enum size=11 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=some offset=0
// CHECK-64:         (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=some offset=0
// CHECK-64:             (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (field name=valid offset=0
// CHECK-64:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=value offset=0
// CHECK-64:                     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                       (field name=_value offset=0
// CHECK-64:                         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_TwoCaseOnePayload.ClassWithTwoCaseOnePayloadEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=55 alignment=4 stride=56 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=some offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=valid offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32:   (field name=e2 offset=16
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=valid offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=value offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=_value offset=0
// CHECK-32:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK-32:   (field name=e3 offset=24
// CHECK-32:     (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=valid offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=value offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=_value offset=0
// CHECK-32:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK-32:   (field name=e4 offset=32
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=some offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=valid offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32:   (field name=e5 offset=40
// CHECK-32:     (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=some offset=0
// CHECK-32:         (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=valid offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=value offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32:   (field name=e6 offset=48
// CHECK-32:     (single_payload_enum size=7 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=some offset=0
// CHECK-32:         (single_payload_enum size=6 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=some offset=0
// CHECK-32:             (single_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (field name=valid offset=0
// CHECK-32:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=value offset=0
// CHECK-32:                     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                       (field name=_value offset=0
// CHECK-32:                         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))))))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
