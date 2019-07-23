// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_nested
// RUN: %target-run %target-swift-reflection-test %t/reflect_nested 2>&1 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class OuterGeneric<T> {
  class Inner {
    class Innermost<U> {
      var x: T
      var y: U

      init(x: T, y: U) {
        self.x = x
        self.y = y
      }
    }
  }
}

var obj = OuterGeneric.Inner.Innermost(x: 17, y: "hello")

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class reflect_nested.OuterGeneric.Inner.Innermost
// CHECK-64-NEXT: (struct Swift.Int)
// CHECK-64-NEXT: (struct Swift.String)

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class reflect_nested.OuterGeneric.Inner.Innermost
// CHECK-32-NEXT: (struct Swift.Int)
// CHECK-32-NEXT: (struct Swift.String)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
