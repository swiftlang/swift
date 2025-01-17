// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Character
// RUN: %target-codesign %t/reflect_Character

// RUN: %target-run %target-swift-reflection-test %t/reflect_Character | %FileCheck %s --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class TestClass {
    var t: Character
    init(t: Character) {
        self.t = t
    }
}

var obj = TestClass(t: "A")

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Character.TestClass)

// CHECK-64-LABEL: Type info:
// CHECK-64: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT: (field name=t offset=16
// CHECK-64-NEXT:   (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:     (field name=_str offset=0
// CHECK-64-NEXT:       (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:         (field name=_guts offset=0
// CHECK-64-NEXT:           (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:             (field name=_object offset=0
// CHECK-64-NEXT:               (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:                 (field name=_countAndFlagsBits offset=0
// CHECK-64-NEXT:                   (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                     (field name=_value offset=0
// CHECK-64-NEXT:                       (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:                 (field name=_object offset=8
// CHECK-64-NEXT:                   (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1)))))))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Character.TestClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=20 alignment=4 stride=20 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=t offset=8
// CHECK-32-NEXT:     (struct size=12 alignment=4 stride=12 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_str offset=0
// CHECK-32-NEXT:         (struct size=12 alignment=4 stride=12 num_extra_inhabitants=253 bitwise_takable=1

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
