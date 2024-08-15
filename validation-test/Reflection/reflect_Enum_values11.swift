// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values11
// RUN: %target-codesign %t/reflect_Enum_values11

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values11 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

public enum A1 {
  case item1
}
public enum A2 {
  case item1
}
public enum A3 {
  case item1
}
public enum A4 {
  case item1
}
public enum A5 {
  case item1
}
public enum A6 {
  case item1
  case item2
}
public enum A7 {
  case item1
  case item2
}

// MemoryLayout<Request>.size = 1
public enum Request {
case a1(A1)
case a2(A2)
case a3(A3)
case a4(A4)
case a5(A5)
case a6(A6)
case a7(A7)
}

reflect(enumValue: Request.a1(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a1(.item1)

reflect(enumValue: Request.a2(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a2(.item1)

reflect(enumValue: Request.a3(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a3(.item1)

reflect(enumValue: Request.a4(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a4(.item1)

reflect(enumValue: Request.a5(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a5(.item1)

reflect(enumValue: Request.a6(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a6(.item1)

reflect(enumValue: Request.a6(.item2))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a6(.item2)

reflect(enumValue: Request.a7(.item1))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a7(.item1)

reflect(enumValue: Request.a7(.item2))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values11.Request)
// CHECKALL-NEXT: Value: .a7(.item2)

doneReflecting()

// CHECKALL: Done.

