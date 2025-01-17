// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Int16
// RUN: %target-codesign %t/reflect_Int16

// RUN: %target-run %target-swift-reflection-test %t/reflect_Int16 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class TestClass {
    var t: Int16
    init(t: Int16) {
        self.t = t
    }
}

var obj = TestClass(t: 123)

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Int16.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=18 alignment=2 stride=18 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Int16.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=10 alignment=2 stride=10 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
