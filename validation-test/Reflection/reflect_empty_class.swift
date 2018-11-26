// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_empty_class
// RUN: %target-codesign %t/reflect_empty_class

// RUN: %target-run %target-swift-reflection-test %t/reflect_empty_class | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

class EmptyClass { }

var obj = EmptyClass()

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_empty_class.EmptyClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=16 alignment=1 stride=16 num_extra_inhabitants=0)

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_empty_class.EmptyClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=8 alignment=1 stride=8 num_extra_inhabitants=0)

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
