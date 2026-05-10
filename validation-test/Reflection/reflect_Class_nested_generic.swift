// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Class_nested_generic.swift
// RUN: %target-codesign %t/reflect_Class_nested_generic.swift

// RUN: %target-run %target-swift-reflection-test %t/reflect_Class_nested_generic.swift | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class HoldsNonNamespacedNestedStruct {
    struct NamespacedNestingStruct<T> {
        let nested: T
    }
    
    let nestedField = NamespacedNestingStruct<Int>(nested: 1)
}

reflect(object: HoldsNonNamespacedNestedStruct())

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (class reflect_Class_nested_generic.HoldsNonNamespacedNestedStruct)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:  (field name=nestedField offset=16
// CHECK-64-NEXT:    (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:      (field name=nested offset=0
// CHECK-64-NEXT:        (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:          (field name=_value offset=0
// CHECK-64-NEXT:            (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

struct NestingStruct<T> {
  let nested: T
}
class HoldsNamespacedNestedStruct {
    let nestedField = NestingStruct<Int>(nested: 1)
}

reflect(object: HoldsNamespacedNestedStruct())

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (class reflect_Class_nested_generic.HoldsNamespacedNestedStruct)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=nestedField offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=nested offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

// CHECK-32: Type info:
// FIXME: actual 32-bit layout here

struct ContainerStruct<T> {
  struct Foo {
    struct Bar {
      struct NestingStruct<U> {
	let t: T
	let u: U
      }
    }
  }
}
class HoldsNamespacedContainedNestedStruct {
  let nestedField = ContainerStruct<Int>.Foo.Bar.NestingStruct<Double>(t: 1, u: 1)
}

reflect(object: HoldsNamespacedContainedNestedStruct())

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (class reflect_Class_nested_generic.HoldsNamespacedContainedNestedStruct)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=nestedField offset=16
// CHECK-64-NEXT:     (struct size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=t offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field name=u offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))


doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
