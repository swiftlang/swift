// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_bulky
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_bulky

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_bulky | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct Q {
  var a = 1
  var b = 2
  var c = 3
  var d = 4

  init(a: Int, b: Int) {
    self.a = a
    self.b = b
  }
}

enum E {
  case A(String,Q,Q?)
  case B(Q,[Q]?)
  case D(Q,Q?)
  case V(Q,Q)
  case X(String,Q,Q)
  case Y(Q,Q?)
}

reflect(enumValue: E.X("hello world", Q(a: 100, b: 200), Q(a: 300, b: 400)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_bulky.E)
// CHECK-NEXT: Value: .X(_)

reflect(enumValue: E.V(Q(a: 100, b: 200), Q(a: 300, b: 400)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_bulky.E)
// CHECK-NEXT: Value: .V(_)

reflect(enumValue: E.A("hello, universe", Q(a: 100, b: 200), Q(a: 300, b: 400)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_bulky.E)
// CHECK-NEXT: Value: .A(_)

doneReflecting()

// CHECK: Done.

