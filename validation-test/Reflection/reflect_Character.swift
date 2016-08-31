// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Character
// RUN: %target-run %target-swift-reflection-test %t/reflect_Character 2>&1 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop
// REQUIRES: executable_test

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
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Character.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=25 alignment=16 stride=32 num_extra_inhabitants=0
// CHECK-64-NEXT:  (field name=t offset=16
// CHECK-64-NEXT:    (struct size=9 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:      (field name=_representation offset=0
// CHECK-64-NEXT:        (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:          (field name=large offset=0
// CHECK-64-NEXT:            (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646
// CHECK-64-NEXT:              (field name=_storage offset=0
// CHECK-64-NEXT:                (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646
// CHECK-64-NEXT:                  (field name=some offset=0
// CHECK-64-NEXT:                    (reference kind=strong refcounting=native))))))
// CHECK-64-NEXT:          (field name=small offset=0
// CHECK-64-NEXT:            (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647)))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Character.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=24 alignment=16 stride=32 num_extra_inhabitants=0
// CHECK-32:   (field name=t offset=16
// CHECK-32:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=_representation offset=0
// CHECK-32:         (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:           (field name=large offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=4095
// CHECK-32:               (field name=_storage offset=0
// CHECK-32:                 (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095
// CHECK-32:                   (field name=some offset=0
// CHECK-32:                     (reference kind=strong refcounting=native))))))
// CHECK-32:           (field name=small offset=0
// CHECK-32:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647)))))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
