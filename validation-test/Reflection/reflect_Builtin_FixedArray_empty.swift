// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -lswiftSwiftReflectionTest %s -o %t/reflect_Builtin_FixedArray_empty
// RUN: %target-codesign %t/reflect_Builtin_FixedArray_empty

// RUN: %target-run %target-swift-reflection-test %t/reflect_Builtin_FixedArray_empty | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

// An empty Builtin.FixedArray occupies no storage, so it has nowhere to hold an
// extra inhabitant no matter how many its element type has. Bool's byte carries
// 254, which a zero-element array must not inherit.
class Holder {
  var empty: InlineArray<0, Bool> = .init(repeating: false)
  var one: InlineArray<1, Bool> = .init(repeating: false)
}

reflect(object: Holder())

// CHECK: Reflecting an object.
// CHECK: Type reference:
// CHECK: (class reflect_Builtin_FixedArray_empty.Holder)

// CHECK: Type info:
// CHECK: (field name=empty
// CHECK: (array size=0 alignment=1 stride=0 num_extra_inhabitants=0 bitwise_takable=1 count=0
// CHECK: (field name=one
// CHECK: (array size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1 count=1

doneReflecting()

// CHECK: Done.
