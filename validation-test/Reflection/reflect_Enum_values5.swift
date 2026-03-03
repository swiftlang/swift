// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values5
// RUN: %target-codesign %t/reflect_Enum_values5

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values5 | tee /dev/stderr | %FileCheck %s --dump-input=fail

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest
public enum E: Error {
  public struct Context {
    public let a: [any CodingKey] = []
    public let b: String = "abc"
    public let c: Error? = nil
  }

  case typeMismatch(Any.Type)
  case valueNotFound(Any.Type, Context)
  case keyNotFound(any CodingKey, Context)
  case dataCorrupted(Context)
}

public enum M {
case A(E)
case B(E, Int)
}

reflect(enumValue: M.A(.typeMismatch(Int.self)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values5.M)
// CHECK-NEXT:  Value: .A(.typeMismatch(_))

reflect(enumValue: M.A(.dataCorrupted(.init())))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values5.M)
// CHECK-NEXT: Value: .A(.dataCorrupted(_))

reflect(enumValue: M.B(.typeMismatch(Int.self), 74))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values5.M)
// Note: reflection tester only drills down into
// payloads that are a single enum.  This payload is a tuple.
// CHECK-NEXT:  Value: .B(_)

reflect(enumValue: M.B(.dataCorrupted(.init()), 42))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT:  (enum reflect_Enum_values5.M)
// CHECK-NEXT: Value: .B(_)

doneReflecting()

// CHECK: Done.

