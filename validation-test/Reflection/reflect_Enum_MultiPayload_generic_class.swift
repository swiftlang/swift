// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic_class
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic_class

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic_class | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

protocol P {}
struct S: P {}

class First<T: P> {}
class Second<T: P> {}

class Outer<T: P> {
  enum Container {
    case first(First<T>)
    case second(Second<T>)
  }

  let container: Container
  weak var parent: First<T>?

  init(first: First<T>) {
    self.container = .first(first)
    self.parent = nil
  }
}

reflect(object: Outer<S>(first: First<S>()))

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_class reflect_Enum_MultiPayload_generic_class.Outer
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic_class.S))

// CHECK: Type info:
// CHECK-64-NEXT: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:   (field name=container offset=16
// CHECK-64-NEXT:     (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-64-NEXT:       (case name=first index=0 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=second index=1 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:   (field name=parent offset=24
// CHECK-64-NEXT:     (reference kind=weak refcounting=native)))

// CHECK-32-NEXT: (class_instance size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:   (field name=container offset=8
// CHECK-32-NEXT:     (multi_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-32-NEXT:       (case name=first index=0 offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))
// CHECK-32-NEXT:       (case name=second index=1 offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:   (field name=parent offset=12
// CHECK-32-NEXT:     (reference kind=weak refcounting=native)))

doneReflecting()

// CHECK: Done.
