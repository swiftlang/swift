// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Float
// RUN: %target-codesign %t/reflect_Float

// RUN: %target-run %target-swift-reflection-test %t/reflect_Float | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class TestClass {
    var t: Float
    init(t: Float) {
        self.t = t
    }
}

var obj = TestClass(t: 123.45)

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Float.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=20 alignment=4 stride=20 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Float.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
