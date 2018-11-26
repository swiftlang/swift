// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Set
// RUN: %target-codesign %t/reflect_Set

// Link %target-swift-reflection-test into %t to convince %target-run to copy
// it.
// RUN: ln -s %target-swift-reflection-test %t/swift-reflection-test
// RUN: %target-run %t/swift-reflection-test %t/reflect_Set | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class TestClass {
    var t: Set<Int>
    init(t: Set<Int>) {
        self.t = t
    }
}

var obj = TestClass(t: [1, 2, 3, 3, 2, 1])

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Set.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// (unstable implementation details omitted)

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Set.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// (unstable implementation details omitted)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
