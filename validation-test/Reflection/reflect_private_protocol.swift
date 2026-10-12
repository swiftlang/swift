// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_private_protocol
// RUN: %target-codesign %t/reflect_private_protocol

// RUN: %target-run %target-swift-reflection-test %t/reflect_private_protocol | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

// Lowering `p` requires the field descriptor of P, and lowering `a` requires
// the associated type descriptor of C: P.

private protocol P { associatedtype A }

private class C: P { typealias A = C }

private class HasPrivateProtocol<T: P> {
  var p: any P = C()
  var a: T.A?
}

reflect(object: HasPrivateProtocol<C>())

// CHECK: Type info:
// CHECK-NEXT: (class_instance
// CHECK-NEXT:   (field name=p
// CHECK-NEXT:     (opaque_existential
// CHECK:        (field name=a
// CHECK-NEXT:     (single_payload_enum
// CHECK-NEXT:       (case name=some
// CHECK-NEXT:         (reference kind=strong refcounting=native))

doneReflecting()

// CHECK: Done.
