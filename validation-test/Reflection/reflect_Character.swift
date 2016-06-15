// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Character
// RUN: %target-run %target-swift-reflection-test %t/reflect_Character 2>&1 | FileCheck %s --check-prefix=CHECK-%target-ptrsize
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
// CHECK-64: <null type info>

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Character.TestClass)

// CHECK-32: Type info:
// CHECK-32: <null type info>

reflect(any: obj)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Character.TestClass)

// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Character.TestClass)

// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
