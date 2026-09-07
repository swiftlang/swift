// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_nested_extra_tag
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_nested_extra_tag

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_nested_extra_tag | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class C {}

// Inner's three payload cases need two tag bits, and its payload has no spare
// bits to hold them, so both go in an extra discriminator byte. The remaining
// six bits of that byte are spare, and Outer takes its own tag from them.
enum Inner {
  case a(C, UInt8)
  case b(C, UInt8)
  case c(C, UInt8)
}

enum Outer {
  case p(Inner)
  case q(Inner)
  case r(Inner)
  case s(Inner)
  case t(Inner)
}

let c = C()

reflect(enumValue: Outer.p(.a(c, 1)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.Outer)
// CHECK-NEXT: Value: .p(.a(_))

reflect(enumValue: Outer.q(.b(c, 2)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.Outer)
// CHECK-NEXT: Value: .q(.b(_))

reflect(enumValue: Outer.r(.c(c, 3)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.Outer)
// CHECK-NEXT: Value: .r(.c(_))

reflect(enumValue: Outer.s(.a(c, 4)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.Outer)
// CHECK-NEXT: Value: .s(.a(_))

reflect(enumValue: Outer.t(.b(c, 5)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.Outer)
// CHECK-NEXT: Value: .t(.b(_))

// An empty case in the inner enum makes its tag spill into the extra
// discriminator without the payload contributing spare bits of its own.
enum InnerWithEmpty {
  case a(AnyObject)
  case b(AnyObject)
  case c
}

enum OuterOfEmpty {
  case x(InnerWithEmpty)
  case y(InnerWithEmpty)
  case z(InnerWithEmpty)
}

reflect(enumValue: OuterOfEmpty.x(.a(c)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.OuterOfEmpty)
// CHECK-NEXT: Value: .x(.a(_))

reflect(enumValue: OuterOfEmpty.y(.b(c)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.OuterOfEmpty)
// CHECK-NEXT: Value: .y(.b(_))

reflect(enumValue: OuterOfEmpty.z(.c))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_nested_extra_tag.OuterOfEmpty)
// CHECK-NEXT: Value: .z(.c)

doneReflecting()

// CHECK: Done.
