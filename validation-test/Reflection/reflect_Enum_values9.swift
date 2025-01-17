// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values9
// RUN: %target-codesign %t/reflect_Enum_values9

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values9 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

enum E : Error {
  case e
}

// MemoryLayout<B>.size == 8
// MemoryLayout<Error>.size == 8
enum B {
case a(Error)
case b(Error)
}

reflect(enumValue: B.a(E.e))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values9.B)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: B.b(E.e))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values9.B)
// CHECKALL-NEXT: Value: .b(_)

doneReflecting()

// CHECKALL: Done.

