// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values6
// RUN: %target-codesign %t/reflect_Enum_values6

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values6 | tee /dev/stderr | %FileCheck %s --dump-input=fail

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

public struct MyError : Error {}

public enum E1 {
case A(Error)
case B(Error)
}

// MemoryLayout<E1>.size == 8  ==>  Error has spare bits

reflect(enumValue: E1.A(MyError()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E1)
// CHECK-NEXT: Value: .A(_)

reflect(enumValue: E1.B(MyError()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E1)
// CHECK-NEXT: Value: .B(_)

public enum E2 {
case A(Error?)
case B(Error?)
}

// MemoryLayout<E2>.size == 9 => Error? has no spare bits

reflect(enumValue: E2.A(MyError()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E2)
// CHECK-NEXT: Value: .A(.some(_))

reflect(enumValue: E2.B(MyError()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E2)
// CHECK-NEXT: Value: .B(.some(_))


public enum E3 {
case A(Any.Type)
case B(Any.Type)
}

// MemoryLayout<E3>.size == 8 => Any.Type has spare bits

reflect(enumValue: E3.A(Any.self))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E3)
// CHECK-NEXT: Value: .A(_)

reflect(enumValue: E3.B(Any.self))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values6.E3)
// CHECK-NEXT: Value: .B(_)

doneReflecting()

// CHECK: Done.

