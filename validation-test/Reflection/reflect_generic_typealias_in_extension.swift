// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_generic_typealias_in_extension
// RUN: %target-codesign %t/reflect_generic_typealias_in_extension

// RUN: %target-run %target-swift-reflection-test %t/reflect_generic_typealias_in_extension | %FileCheck %s

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

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class {{.*}}TypealiasHolder
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=pair
// CHECK: (tuple
// CHECK: (field offset=0
// CHECK: (struct size=8
// CHECK: (field name=_value
// CHECK: (field offset=8
// CHECK: (struct size=16
// CHECK: (field name=_guts

doneReflecting()

// CHECK: Done.
