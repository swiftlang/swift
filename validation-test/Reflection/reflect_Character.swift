// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Character
// RUN: %target-codesign %t/reflect_Character

// RUN: %target-run %target-swift-reflection-test %t/reflect_Character | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

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
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Character.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:  (field name=t offset=16
// CHECK-64-NEXT:    (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:      (field name=_representation offset=0
// CHECK-64-NEXT:        (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:          (field name=smallUTF16 offset=0
// CHECK-64-NEXT:            (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647))
// CHECK-64-NEXT:          (field name=large offset=0
// CHECK-64-NEXT:            (reference kind=strong refcounting=native)))))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Character.TestClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=t offset=8
// CHECK-32-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=_representation offset=0
// CHECK-32-NEXT:         (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field name=smallUTF16 offset=0
// CHECK-32-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647))
// CHECK-32-NEXT:           (field name=large offset=0
// CHECK-32-NEXT:            (reference kind=strong refcounting=native)))))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
