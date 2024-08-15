// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values10
// RUN: %target-codesign %t/reflect_Enum_values10

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values10 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

protocol P : AnyObject {
}

class C : P {
  var a: Int
  var b: Int
  init() { a = 0; b = 0; }
}

// MemoryLayout<B>.size == 8
enum B {
case a(C)
case b(C)
}

reflect(enumValue: B.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: B.b(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B)
// CHECKALL-NEXT: Value: .b(_)

// MemoryLayout<Q>.size == 16
// MemoryLayout<P>.size == 16
enum Q {
case a(P)
case b(P)
}

reflect(enumValue: Q.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: Q.b(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q)
// CHECKALL-NEXT: Value: .b(_)

doneReflecting()

// CHECKALL: Done.

