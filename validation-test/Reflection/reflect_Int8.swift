// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Int8
// RUN: %target-codesign %t/reflect_Int8

// RUN: %target-run %target-swift-reflection-test %t/reflect_Int8 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class TestClass {
    var t: Int8
    init(t: Int8) {
        self.t = t
    }
}

var obj = TestClass(t: 123)

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Int8.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=17 alignment=1 stride=17 num_extra_inhabitants=0
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Int8.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=9 alignment=1 stride=9 num_extra_inhabitants=0
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
