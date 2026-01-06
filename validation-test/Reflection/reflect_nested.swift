// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_nested
// RUN: %target-codesign %t/reflect_nested
// RUN: %target-run %target-swift-reflection-test %t/reflect_nested 2>&1 | %FileCheck %s --check-prefix=CHECK
// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

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

// CHECK: Reflecting an object.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_class reflect_nested.OuterGeneric.Inner.Innermost
// CHECK-NEXT:   (struct Swift.String)
// CHECK-NEXT:   (bound_generic_class reflect_nested.OuterGeneric
// CHECK-NEXT:     (struct Swift.Int)))

class OuterNonGeneric {
  class InnerGeneric<T, U> {
    var x: T
    var y: U

    init(x: T, y: U) {
      self.x = x
      self.y = y
    }
  }
}

var obj2 = OuterNonGeneric.InnerGeneric(x: 17, y: "hello")
reflect(object: obj2)

// CHECK: Reflecting an object.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_class reflect_nested.OuterNonGeneric.InnerGeneric
// CHECK-NEXT:   (struct Swift.Int)
// CHECK-NEXT:   (struct Swift.String))

doneReflecting()

// CHECK: Done.
