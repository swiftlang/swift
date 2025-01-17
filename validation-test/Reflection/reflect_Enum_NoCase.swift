// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_NoCase
// RUN: %target-codesign %t/reflect_Enum_NoCase

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_NoCase | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

enum NoCaseEnum {
}

class ClassWithNoCaseEnum {
  var e1: NoCaseEnum?
  var e2: NoCaseEnum??
  var e3: NoCaseEnum???
  var e4: NoCaseEnum????
  var e5: NoCaseEnum?????
}

reflect(object: ClassWithNoCaseEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_NoCase.ClassWithNoCaseEnum)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=31 alignment=1 stride=31 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=e1 offset=16
// CHECK-64:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e2 offset=17
// CHECK-64:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e3 offset=19
// CHECK-64:     (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (case name=some index=0 offset=0
// CHECK-64:                 (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:               (case name=none index=1)))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e4 offset=22
// CHECK-64:     (single_payload_enum size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (case name=some index=0 offset=0
// CHECK-64:                 (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (case name=some index=0 offset=0
// CHECK-64:                     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:                   (case name=none index=1)))
// CHECK-64:               (case name=none index=1)))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=e5 offset=26
// CHECK-64:     (single_payload_enum size=5 alignment=1 stride=5 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (single_payload_enum size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (case name=some index=0 offset=0
// CHECK-64:             (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:               (case name=some index=0 offset=0
// CHECK-64:                 (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (case name=some index=0 offset=0
// CHECK-64:                     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                       (case name=some index=0 offset=0
// CHECK-64:                         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:                       (case name=none index=1)))
// CHECK-64:                   (case name=none index=1)))
// CHECK-64:               (case name=none index=1)))
// CHECK-64:           (case name=none index=1)))
// CHECK-64:       (case name=none index=1))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_NoCase.ClassWithNoCaseEnum)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=23 alignment=1 stride=23 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=e1 offset=8
// CHECK-32:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e2 offset=9
// CHECK-32:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e3 offset=11
// CHECK-32:     (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (case name=some index=0 offset=0
// CHECK-32:                 (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:               (case name=none index=1)))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e4 offset=14
// CHECK-32:     (single_payload_enum size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (case name=some index=0 offset=0
// CHECK-32:                 (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (case name=some index=0 offset=0
// CHECK-32:                     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:                   (case name=none index=1)))
// CHECK-32:               (case name=none index=1)))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=e5 offset=18
// CHECK-32:     (single_payload_enum size=5 alignment=1 stride=5 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (single_payload_enum size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (case name=some index=0 offset=0
// CHECK-32:             (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:               (case name=some index=0 offset=0
// CHECK-32:                 (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (case name=some index=0 offset=0
// CHECK-32:                     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                       (case name=some index=0 offset=0
// CHECK-32:                         (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:                   (case name=none index=1)))
// CHECK-32:               (case name=none index=1)))
// CHECK-32:           (case name=none index=1)))
// CHECK-32:       (case name=none index=1))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
