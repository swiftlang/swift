// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple -lswiftSwiftReflectionTest %s -o %t/reflect_variadic_generic
// RUN: %target-codesign %t/reflect_variadic_generic

// RUN: %target-run %target-swift-reflection-test %t/reflect_variadic_generic | tee /dev/stderr | %FileCheck %s --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct VariadicStruct<each T> {
  var values: (repeat each T)
}

class VariadicHolder<each T> {
  var contents: (repeat each T)
  init(_ values: repeat each T) {
    contents = (repeat each values)
  }
}

// Empty pack.
reflect(any: VariadicStruct< >(values: ()))

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (bound_generic_struct reflect_variadic_generic.VariadicStruct
// CHECK-NEXT: (pack))

// One-element pack.
reflect(any: VariadicStruct<Int>(values: 5))

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (bound_generic_struct reflect_variadic_generic.VariadicStruct
// CHECK-NEXT: (pack
// CHECK-NEXT: (struct Swift.Int)))

// Multi-element pack with mixed element types.
reflect(any: VariadicStruct<Int, String, Bool>(values: (1, "two", true)))

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (bound_generic_struct reflect_variadic_generic.VariadicStruct
// CHECK-NEXT: (pack
// CHECK-NEXT: (struct Swift.Int)
// CHECK-NEXT: (struct Swift.String)
// CHECK-NEXT: (struct Swift.Bool)))

// Class with parameter pack.
reflect(object: VariadicHolder<Int, Double>(7, 3.14))

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (bound_generic_class reflect_variadic_generic.VariadicHolder
// CHECK-NEXT: (pack
// CHECK-NEXT: (struct Swift.Int)
// CHECK-NEXT: (struct Swift.Double)))

doneReflecting()

// CHECK: Done.
