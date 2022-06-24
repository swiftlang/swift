// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

public enum EnumMultipleElementsInSingleCase { case first, second }

public enum EnumSingleElementInSingleCase {
    case first
    case second
}

public enum EnumCaseIsSwiftKeyword {
    case first(a: Int)
    case `protocol`(b: Double)
}

public enum EnumCaseIsCxxKeyword {
    case first, second(x: Float)
    case const
}

// CHECK: class EnumCaseIsCxxKeyword final {
// CHECK:      enum class cases {
// CHECK-NEXT:   first,
// CHECK-NEXT:   second,
// CHECK-NEXT:   const_
// CHECK-NEXT: };

// CHECK: class EnumCaseIsSwiftKeyword final {
// CHECK:      enum class cases {
// CHECK-NEXT:   first,
// CHECK-NEXT:   protocol
// CHECK-NEXT: };

// CHECK: class EnumMultipleElementsInSingleCase final {
// CHECK:      enum class cases {
// CHECK-NEXT:   first,
// CHECK-NEXT:   second
// CHECK-NEXT: };

// CHECK: class EnumSingleElementInSingleCase final {
// CHECK:      enum class cases {
// CHECK-NEXT:   first,
// CHECK-NEXT:   second
// CHECK-NEXT: };
