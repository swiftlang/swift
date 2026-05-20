// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_InlineArray -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/reflect_InlineArray

// RUN: %target-run %target-swift-reflection-test %t/reflect_InlineArray | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

import SwiftReflectionTest

class TestClass {
    var t: InlineArray<42, Int>
    var u: InlineArray<42, Int>
    var v: InlineArray<42, Int>
    var bytes: InlineArray<7, UInt8>
    var nested: InlineArray<3, InlineArray<5, Int16>>
    init(t: InlineArray<42, Int>) {
        self.t = t
        self.u = t
        self.v = t
        self.bytes = .init(repeating: 0)
        self.nested = .init(repeating: .init(repeating: 0))
    }
}

var obj = TestClass(t: .init(repeating: 42))

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_InlineArray.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size={{[0-9]+}} alignment=8 stride={{[0-9]+}} num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=t offset={{[0-9]+}}
// CHECK-64:     (struct size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_storage offset=0
// CHECK-64:         (array size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-64:           (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:             (field name=_value offset=0
// CHECK-64:               (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64:   (field name=u offset={{[0-9]+}}
// CHECK-64:     (struct size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_storage offset=0
// CHECK-64:         (array size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-64:           (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:             (field name=_value offset=0
// CHECK-64:               (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64:   (field name=v offset={{[0-9]+}}
// CHECK-64:     (struct size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_storage offset=0
// CHECK-64:         (array size=336 alignment=8 stride=336 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-64:           (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:             (field name=_value offset=0
// CHECK-64:               (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64:   (field name=bytes offset={{[0-9]+}}
// CHECK-64:     (struct size=7 alignment=1 stride=7 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_storage offset=0
// CHECK-64:         (array size=7 alignment=1 stride=7 num_extra_inhabitants=0 bitwise_takable=1 count=7
// CHECK-64:           (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:             (field name=_value offset=0
// CHECK-64:               (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64:   (field name=nested offset={{[0-9]+}}
// CHECK-64:     (struct size=30 alignment=2 stride=30 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_storage offset=0
// CHECK-64:         (array size=30 alignment=2 stride=30 num_extra_inhabitants=0 bitwise_takable=1 count=3
// CHECK-64:           (struct size=10 alignment=2 stride=10 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:             (field name=_storage offset=0
// CHECK-64:               (array size=10 alignment=2 stride=10 num_extra_inhabitants=0 bitwise_takable=1 count=5
// CHECK-64:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:                   (field name=_value offset=0
// CHECK-64:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1)))))))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_InlineArray.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size={{[0-9]+}} alignment={{[0-9]+}} stride={{[0-9]+}} num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-32:   (field name=t offset={{[0-9]+}}
// CHECK-32:     (struct size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_storage offset=0
// CHECK-32:         (array size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-32:           (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:             (field name=_value offset=0
// CHECK-32:               (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32:   (field name=u offset={{[0-9]+}}
// CHECK-32:     (struct size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_storage offset=0
// CHECK-32:         (array size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-32:           (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:             (field name=_value offset=0
// CHECK-32:               (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32:   (field name=v offset={{[0-9]+}}
// CHECK-32:     (struct size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_storage offset=0
// CHECK-32:         (array size=168 alignment=4 stride=168 num_extra_inhabitants=0 bitwise_takable=1 count=42
// CHECK-32:           (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:             (field name=_value offset=0
// CHECK-32:               (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32:   (field name=bytes offset={{[0-9]+}}
// CHECK-32:     (struct size=7 alignment=1 stride=7 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_storage offset=0
// CHECK-32:         (array size=7 alignment=1 stride=7 num_extra_inhabitants=0 bitwise_takable=1 count=7
// CHECK-32:           (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:             (field name=_value offset=0
// CHECK-32:               (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32:   (field name=nested offset={{[0-9]+}}
// CHECK-32:     (struct size=30 alignment=2 stride=30 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_storage offset=0
// CHECK-32:         (array size=30 alignment=2 stride=30 num_extra_inhabitants=0 bitwise_takable=1 count=3
// CHECK-32:           (struct size=10 alignment=2 stride=10 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:             (field name=_storage offset=0
// CHECK-32:               (array size=10 alignment=2 stride=10 num_extra_inhabitants=0 bitwise_takable=1 count=5
// CHECK-32:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:                   (field name=_value offset=0
// CHECK-32:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1)))))))))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
