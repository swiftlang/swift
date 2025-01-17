// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values7
// RUN: %target-codesign %t/reflect_Enum_values7

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values7 | tee /dev/stderr | %FileCheck %s --dump-input=fail

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

public struct MyError : Error {}

// MemoryLayout<MPEWithClosurePayload>.size == 17
// MemoryLayout<()->Void>.size == 16
enum MPEWithClosurePayload {
    case int(()->Void)
    case closure(() -> Void)
}

// MemoryLayout<E>.size == 17
// enum E { case a(()->Void); case b(()->Void); }

reflect(enumValue: MPEWithClosurePayload.int({}))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_values7.MPEWithClosurePayload)
// CHECK-NEXT: Value: .int(_)

reflect(enumValue: MPEWithClosurePayload.closure({}))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_values7.MPEWithClosurePayload)
// CHECK-NEXT: Value: .closure(_)

doneReflecting()

// CHECK: Done.

