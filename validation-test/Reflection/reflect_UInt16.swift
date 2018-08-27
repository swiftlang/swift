// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_UInt16
// RUN: %target-codesign %t/reflect_UInt16

// Link %target-swift-reflection-test into %t to convince %target-run to copy
// it.
// RUN: ln -s %target-swift-reflection-test %t/swift-reflection-test
// RUN: %target-run %t/swift-reflection-test %t/reflect_UInt16 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class TestClass {
    var t: UInt16
    init(t: UInt16) {
        self.t = t
    }
}

var obj = TestClass(t: 123)

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_UInt16.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=18 alignment=2 stride=18 num_extra_inhabitants=0
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_UInt16.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=10 alignment=2 stride=10 num_extra_inhabitants=0
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
