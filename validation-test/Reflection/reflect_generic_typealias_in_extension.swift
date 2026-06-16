// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_generic_typealias_in_extension
// RUN: %target-codesign %t/reflect_generic_typealias_in_extension

// RUN: %target-run %target-swift-reflection-test %t/reflect_generic_typealias_in_extension | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct Outer<T> {}

extension Outer {
  typealias Pair<U> = (T, U)
}

class TypealiasHolder {
  var pair: Outer<Int>.Pair<String> = (1, "hi")
}

reflect(object: TypealiasHolder())

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class {{.*}}TypealiasHolder
// CHECK-64: Type info:
// CHECK-64: (class_instance
// CHECK-64: (field name=pair
// CHECK-64: (tuple
// CHECK-64: (field offset=0
// CHECK-64: (struct size=8
// CHECK-64: (field name=_value
// CHECK-64: (field offset=8
// CHECK-64: (struct size=16
// CHECK-64: (field name=_guts

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class {{.*}}TypealiasHolder
// CHECK-32: Type info:
// CHECK-32: (class_instance
// CHECK-32: (field name=pair
// CHECK-32: (tuple
// CHECK-32: (field offset=0
// CHECK-32: (struct size=4
// CHECK-32: (field name=_value
// CHECK-32: (field offset=4
// CHECK-32: (struct size=12
// CHECK-32: (field name=_guts

doneReflecting()

// CHECK-64: Done.
// CHECK-32: Done.
