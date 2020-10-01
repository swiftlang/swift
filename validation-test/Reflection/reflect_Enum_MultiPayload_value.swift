// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_value
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_value

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_value | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest

class ClassA { let a = 7 }
class ClassB { let b = 8 }
enum Either<T,U> {
case left(T)
case right(U)
}

reflect(enumValue: Either<Int,Double>.left(7))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:   (struct Swift.Int)
// CHECK-NEXT:   (struct Swift.Double))
// CHECK-NEXT: Value: .left(_)

reflect(enumValue: Either<Int,Double>.right(1.0))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:   (struct Swift.Int)
// CHECK-NEXT:   (struct Swift.Double))
// CHECK-NEXT: Value: .right(_)

reflect(enumValue: Either<Int,Double>?.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:     (struct Swift.Int)
// CHECK-NEXT:     (struct Swift.Double)))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<Int,Double>??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:       (struct Swift.Int)
// CHECK-NEXT:       (struct Swift.Double))))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<Int,Double>???.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum Swift.Optional
// CHECK-NEXT:       (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:         (struct Swift.Int)
// CHECK-NEXT:         (struct Swift.Double)))))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<Int,Double>????.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum Swift.Optional
// CHECK-NEXT:       (bound_generic_enum Swift.Optional
// CHECK-NEXT:         (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:           (struct Swift.Int)
// CHECK-NEXT:           (struct Swift.Double))))))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<ClassA,ClassB>.left(ClassA()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .left(_)

reflect(enumValue: Either<ClassA,ClassB>.right(ClassB()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .right(_)

reflect(enumValue: Either<ClassA,ClassB>?.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:     (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:     (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<ClassA,ClassB>??.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:       (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:       (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<ClassA,ClassB>???.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum Swift.Optional
// CHECK-NEXT:       (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:         (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:         (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .none

reflect(enumValue: Either<ClassA,ClassB>????.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (bound_generic_enum Swift.Optional
// CHECK-NEXT:       (bound_generic_enum Swift.Optional
// CHECK-NEXT:         (bound_generic_enum reflect_Enum_MultiPayload_value.Either
// CHECK-NEXT:           (class reflect_Enum_MultiPayload_value.ClassA)
// CHECK-NEXT:           (class reflect_Enum_MultiPayload_value.ClassB))
// CHECK-NEXT: Value: .none

doneReflecting()

// CHECK: Done.

