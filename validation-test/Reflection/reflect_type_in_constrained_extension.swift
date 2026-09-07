// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_type_in_constrained_extension
// RUN: %target-codesign %t/reflect_type_in_constrained_extension

// RUN: %target-run %target-swift-reflection-test %t/reflect_type_in_constrained_extension | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct S<T> {
  let t: T
}

extension S<Int> {
  struct Concretized {
    let i = 42
  }
}

extension S where T: Equatable {
  struct Constrained {
    let j = 7
  }
}

reflect(any: S<Int>.Concretized())

// CHECK-64: Reflecting an existential.
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_struct {{.*}}Concretized
// CHECK-64:   (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (struct size=8 alignment=8 stride=8
// CHECK-64:   (field name=i offset=0
// CHECK-64:     (struct size=8

// CHECK-32: Reflecting an existential.
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_struct {{.*}}Concretized
// CHECK-32:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (struct size=4 alignment=4 stride=4
// CHECK-32:   (field name=i offset=0
// CHECK-32:     (struct size=4

reflect(any: S<Int>.Constrained())

// CHECK-64: Reflecting an existential.
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_struct {{.*}}Constrained
// CHECK-64:   (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (struct size=8 alignment=8 stride=8
// CHECK-64:   (field name=j offset=0
// CHECK-64:     (struct size=8

// CHECK-32: Reflecting an existential.
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_struct {{.*}}Constrained
// CHECK-32:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (struct size=4 alignment=4 stride=4
// CHECK-32:   (field name=j offset=0
// CHECK-32:     (struct size=4

doneReflecting()

// CHECK-64: Done.
// CHECK-32: Done.
