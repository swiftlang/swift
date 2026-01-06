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

// On 64-bit: MemoryLayout<B>.size == 8
// On 32-bit: MemoryLayout<B>.size == 4
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

// On 64-bit: MemoryLayout<Q>.size == MemoryLayout<P>.size == 16
// On 32-bit: MemoryLayout<Q>.size == MemoryLayout<P>.size == 8
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

enum B1 {
case a(C)
case b
}

reflect(enumValue: B1.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B1)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: B1.b)

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B1)
// CHECKALL-NEXT: Value: .b

enum Q1 {
case a(P)
case b
}

reflect(enumValue: Q1.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q1)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: Q1.b)

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q1)
// CHECKALL-NEXT: Value: .b

enum B2 {
case a(C)
case b(C)
case c(C)
case d(C)
case e(C)
}

reflect(enumValue: B2.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B2)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: B2.e(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.B2)
// CHECKALL-NEXT: Value: .e(_)

// On 64-bit: MemoryLayout<Q>.size == MemoryLayout<P>.size == 16
// On 32-bit: MemoryLayout<Q>.size == MemoryLayout<P>.size == 8
enum Q2 {
case a(P)
case b(P)
case c(P)
case d(P)
case e(P)
}

reflect(enumValue: Q2.a(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q2)
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: Q2.e(C()))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values10.Q2)
// CHECKALL-NEXT: Value: .e(_)

reflect(enumValue: Optional<Q2>.some(.a(C())))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (enum reflect_Enum_values10.Q2))
// CHECKALL-NEXT: Value: .some(.a(_))

reflect(enumValue: Optional<Q2>.some(.e(C())))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (enum reflect_Enum_values10.Q2))
// CHECKALL-NEXT: Value: .some(.e(_))

reflect(enumValue: Optional<Q2>.none)

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (enum reflect_Enum_values10.Q2))
// CHECKALL-NEXT: Value: .none

doneReflecting()

// CHECKALL: Done.

