// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_large
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_large

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_large | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class ClassA {}
class ClassB {}
class ClassC {}
class ClassD {}
class ClassE {}
class ClassF {}

enum LargeMPE1 {
case AA(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case AB(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case AC(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case AD(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case AE(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case AF(ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
case BA(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case BB(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case BC(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case BD(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case BE(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case BF(ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
case CA(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case CB(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case CC(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case CD(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case CE(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case CF(ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
case DA(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case DB(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case DC(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case DD(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case DE(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case DF(ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
case EA(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case EB(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case EC(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case ED(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case EE(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case EF(ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
case FA(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA)
case FB(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB)
case FC(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC)
case FD(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD)
case FE(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE)
case FF(ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF)
}

reflect(enumValue: LargeMPE1.CD(ClassC(), 1, 2, 3, 4, 5, 6, 7, 8, ClassD()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE1)
// CHECK-NEXT: Value: .CD(_)

reflect(enumValue: LargeMPE1??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE1)))
// CHECK-NEXT: Value: .none

reflect(enumValue: LargeMPE1??.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE1)))
// CHECK-NEXT: Value: .some(.none)



// Pointers are at the end, so there are no reusable spare bits
// in the first part of the enum.
enum LargeMPE2 {
case AA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassA)
case AB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassB)
case AC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassC)
case AD(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassD)
case AE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassE)
case AF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassA, ClassF)
case BA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassA)
case BB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassB)
case BC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassC)
case BD(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassD)
case BE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassE)
case BF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassB, ClassF)
case CA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassA)
case CB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassB)
case CC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassC)
case CD(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassD)
case CE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassE)
case CF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassC, ClassF)
case DA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassA)
case DB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassB)
case DC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassC)
case DD(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassD)
case DE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassE)
case DF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassD, ClassF)
case EA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassA)
case EB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassB)
case EC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassC)
case ED(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassD)
case EE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassE)
case EF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassE, ClassF)
case FA(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassA)
case FB(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassB)
case FC(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassC)
case FD(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassD)
case FE(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassE)
case FF(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, ClassF, ClassF)
}

reflect(enumValue: LargeMPE2.CD(1, 2, 3, 4, 5, 6, 7, 8, ClassC(), ClassD()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE2)
// CHECK-NEXT: Value: .CD(_)

reflect(enumValue: LargeMPE2??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE2)))
// CHECK-NEXT: Value: .none

reflect(enumValue: LargeMPE2??.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE2)))
// CHECK-NEXT: Value: .some(.none)


// Pointers are at the beginning, so there are no reusable spare bits
// in the last part of the enum.
enum LargeMPE3 {
case AA(ClassA, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case AB(ClassA, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case AC(ClassA, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case AD(ClassA, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case AE(ClassA, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case AF(ClassA, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BA(ClassB, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BB(ClassB, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BC(ClassB, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BD(ClassB, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BE(ClassB, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case BF(ClassB, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CA(ClassC, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CB(ClassC, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CC(ClassC, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CD(ClassC, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CE(ClassC, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case CF(ClassC, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DA(ClassD, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DB(ClassD, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DC(ClassD, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DD(ClassD, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DE(ClassD, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case DF(ClassD, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case EA(ClassE, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case EB(ClassE, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case EC(ClassE, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case ED(ClassE, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case EE(ClassE, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case EF(ClassE, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FA(ClassF, ClassA, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FB(ClassF, ClassB, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FC(ClassF, ClassC, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FD(ClassF, ClassD, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FE(ClassF, ClassE, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
case FF(ClassF, ClassF, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
}

reflect(enumValue: LargeMPE3.CD(ClassC(), ClassD(), 1, 2, 3, 4, 5, 6, 7, 8))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE3)
// CHECK-NEXT: Value: .CD(_)

reflect(enumValue: LargeMPE3??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE3)))
// CHECK-NEXT: Value: .none

reflect(enumValue: LargeMPE3??.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE3)))
// CHECK-NEXT: Value: .some(.none)


// Pointers are in the middle.
enum LargeMPE4 {
case AA(Int32, Int32, Int32, Int32, ClassA, ClassA, Int32, Int32, Int32, Int32)
case AB(Int32, Int32, Int32, Int32, ClassA, ClassB, Int32, Int32, Int32, Int32)
case AC(Int32, Int32, Int32, Int32, ClassA, ClassC, Int32, Int32, Int32, Int32)
case AD(Int32, Int32, Int32, Int32, ClassA, ClassD, Int32, Int32, Int32, Int32)
case AE(Int32, Int32, Int32, Int32, ClassA, ClassE, Int32, Int32, Int32, Int32)
case AF(Int32, Int32, Int32, Int32, ClassA, ClassF, Int32, Int32, Int32, Int32)
case BA(Int32, Int32, Int32, Int32, ClassB, ClassA, Int32, Int32, Int32, Int32)
case BB(Int32, Int32, Int32, Int32, ClassB, ClassB, Int32, Int32, Int32, Int32)
case BC(Int32, Int32, Int32, Int32, ClassB, ClassC, Int32, Int32, Int32, Int32)
case BD(Int32, Int32, Int32, Int32, ClassB, ClassD, Int32, Int32, Int32, Int32)
case BE(Int32, Int32, Int32, Int32, ClassB, ClassE, Int32, Int32, Int32, Int32)
case BF(Int32, Int32, Int32, Int32, ClassB, ClassF, Int32, Int32, Int32, Int32)
case CA(Int32, Int32, Int32, Int32, ClassC, ClassA, Int32, Int32, Int32, Int32)
case CB(Int32, Int32, Int32, Int32, ClassC, ClassB, Int32, Int32, Int32, Int32)
case CC(Int32, Int32, Int32, Int32, ClassC, ClassC, Int32, Int32, Int32, Int32)
case CD(Int32, Int32, Int32, Int32, ClassC, ClassD, Int32, Int32, Int32, Int32)
case CE(Int32, Int32, Int32, Int32, ClassC, ClassE, Int32, Int32, Int32, Int32)
case CF(Int32, Int32, Int32, Int32, ClassC, ClassF, Int32, Int32, Int32, Int32)
case DA(Int32, Int32, Int32, Int32, ClassD, ClassA, Int32, Int32, Int32, Int32)
case DB(Int32, Int32, Int32, Int32, ClassD, ClassB, Int32, Int32, Int32, Int32)
case DC(Int32, Int32, Int32, Int32, ClassD, ClassC, Int32, Int32, Int32, Int32)
case DD(Int32, Int32, Int32, Int32, ClassD, ClassD, Int32, Int32, Int32, Int32)
case DE(Int32, Int32, Int32, Int32, ClassD, ClassE, Int32, Int32, Int32, Int32)
case DF(Int32, Int32, Int32, Int32, ClassD, ClassF, Int32, Int32, Int32, Int32)
case EA(Int32, Int32, Int32, Int32, ClassE, ClassA, Int32, Int32, Int32, Int32)
case EB(Int32, Int32, Int32, Int32, ClassE, ClassB, Int32, Int32, Int32, Int32)
case EC(Int32, Int32, Int32, Int32, ClassE, ClassC, Int32, Int32, Int32, Int32)
case ED(Int32, Int32, Int32, Int32, ClassE, ClassD, Int32, Int32, Int32, Int32)
case EE(Int32, Int32, Int32, Int32, ClassE, ClassE, Int32, Int32, Int32, Int32)
case EF(Int32, Int32, Int32, Int32, ClassE, ClassF, Int32, Int32, Int32, Int32)
case FA(Int32, Int32, Int32, Int32, ClassF, ClassA, Int32, Int32, Int32, Int32)
case FB(Int32, Int32, Int32, Int32, ClassF, ClassB, Int32, Int32, Int32, Int32)
case FC(Int32, Int32, Int32, Int32, ClassF, ClassC, Int32, Int32, Int32, Int32)
case FD(Int32, Int32, Int32, Int32, ClassF, ClassD, Int32, Int32, Int32, Int32)
case FE(Int32, Int32, Int32, Int32, ClassF, ClassE, Int32, Int32, Int32, Int32)
case FF(Int32, Int32, Int32, Int32, ClassF, ClassF, Int32, Int32, Int32, Int32)
}

reflect(enumValue: LargeMPE4.CD(1, 2, 3, 4, ClassC(), ClassD(), 5, 6, 7, 8))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE4)
// CHECK-NEXT: Value: .CD(_)

reflect(enumValue: LargeMPE4??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE4)))
// CHECK-NEXT: Value: .none

reflect(enumValue: LargeMPE4??.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_large.LargeMPE4)))
// CHECK-NEXT: Value: .some(.none)



doneReflecting()

// CHECK: Done.

