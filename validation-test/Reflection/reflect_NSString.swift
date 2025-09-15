// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_NSString
// RUN: %target-codesign %t/reflect_NSString

// RUN: %target-run %target-swift-reflection-test %t/reflect_NSString | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest
import Foundation

class TestClass {
    var t: NSString
    init(t: NSString) {
        self.t = t
    }
}

var obj = TestClass(t: "Hello, NSString!")

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_NSString.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=t offset=16
// CHECK-64:     (reference kind=strong refcounting=unknown)))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_NSString.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=t offset=8
// CHECK-32:     (reference kind=strong refcounting=unknown)))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
