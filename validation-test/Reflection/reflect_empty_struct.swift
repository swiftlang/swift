// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest -I %S/Inputs/EmptyStruct/ %s -o %t/reflect_empty_struct
// RUN: %target-codesign %t/reflect_empty_struct

// RUN: %target-run %target-swift-reflection-test %t/reflect_empty_struct | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --dump-input fail

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

import EmptyStruct

@_alignment(1) struct EmptyStruct { }
class Class {
  var a = EmptyStruct()
  var b: Any = EmptyStruct()
  var c = EmptyStructC()
  var d: Any = EmptyStructC()
}

var obj = Class()

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_empty_struct.Class)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=80 alignment=8 stride=80 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=a offset=16
// CHECK-64:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:   (field name=b offset=16
// CHECK-64:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))
// CHECK-64:   (field name=c offset=48
// CHECK-64:     (builtin size=0 alignment=4 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64:   (field name=d offset=48
// CHECK-64:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_empty_struct.Class)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=40 alignment=4 stride=40 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:   (field name=a offset=8
// CHECK-32:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:   (field name=b offset=8
// CHECK-32:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:   (field name=c offset=24
// CHECK-32:     (builtin size=0 alignment=4 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32:   (field name=d offset=24
// CHECK-32:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
