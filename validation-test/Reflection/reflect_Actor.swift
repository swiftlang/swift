// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Actor.swift
// RUN: %target-codesign %t/reflect_Actor.swift

// RUN: %target-run %target-swift-reflection-test %t/reflect_Actor.swift | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

@available(SwiftStdlib 5.1, *)
actor MyActor {
  var i: Int
  init() { i = 1 }
}

if #available(SwiftStdlib 5.1, *) {
  let actor = MyActor()
  reflect(object: actor)
}

// CHECK-64: Reflecting an object.
// CHECK-64-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64-NEXT: Type reference:
// CHECK-64-NEXT: (class reflect_Actor.MyActor)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=120 alignment=16 stride=128 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=$defaultActor offset=16
// CHECK-64-NEXT:     (builtin size=96 alignment=16 stride=96 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:   (field name=i offset=112
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32-NEXT: Type reference:
// CHECK-32-NEXT: (class reflect_Actor.MyActor)

// CHECK-32: Type info:
// FIXME: actual 32-bit layout here

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
