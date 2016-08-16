// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_String
// RUN: %target-run %target-swift-reflection-test %t/reflect_String 2>&1 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class TestClass {
    var t: String
    init(t: String) {
        self.t = t
    }
}

var obj = TestClass(t: "Hello, Reflection!")

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_String.TestClass)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=40 alignment=16 stride=48
// CHECK-64-NEXT:   (field name=t offset=16
// CHECK-64-NEXT:     (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=_core offset=0
// CHECK-64-NEXT:         (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field name=_baseAddress offset=0
// CHECK-64-NEXT:             (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field name=some offset=0
// CHECK-64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=1
// CHECK-64-NEXT:                   (field name=_rawValue offset=0
// CHECK-64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))))
// CHECK-64-NEXT:           (field name=_countAndFlags offset=8
// CHECK-64-NEXT:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64-NEXT:           (field name=_owner offset=16
// CHECK-64-NEXT:             (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646
// CHECK-64-NEXT:               (field name=some offset=0
// CHECK-64-NEXT:                 (class_existential size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647
// CHECK-64-NEXT:                   (field name=object offset=0
// CHECK-64-NEXT:                     (reference kind=strong refcounting=unknown)))))))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_String.TestClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=24 alignment=16 stride=32
// CHECK-32-NEXT:   (field name=t offset=12
// CHECK-32-NEXT:     (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=_core offset=0
// CHECK-32-NEXT:         (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field name=_baseAddress offset=0
// CHECK-32-NEXT:             (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field name=some offset=0
// CHECK-32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=1
// CHECK-32-NEXT:                   (field name=_rawValue offset=0
// CHECK-32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))))
// CHECK-32-NEXT:           (field name=_countAndFlags offset=4
// CHECK-32-NEXT:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32-NEXT:           (field name=_owner offset=8
// CHECK-32-NEXT:             (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095
// CHECK-32-NEXT:               (field name=some offset=0
// CHECK-32-NEXT:                 (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096
// CHECK-32-NEXT:                   (field name=object offset=0
// CHECK-32-NEXT:                     (reference kind=strong refcounting=unknown)))))))))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
