// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-c-header-in-clang(%t/structs.h)

public struct StructSeveralI64 {
    let x1, x2, x3, x4, x5: Int64
}
// CHECK:      struct Structs_StructSeveralI64 {
// CHECK-NEXT:   _Alignas(8) char _storage[40];
// CHECK-NEXT: };

// CHECK-NOT: swift_interop

public func returnNewStructSeveralI64(i: Int64) -> StructSeveralI64 {
    return StructSeveralI64(x1: i, x2: 0, x3: -17, x4: 12345612, x5: -0xFFFF)
}

public func passThroughStructSeveralI64(i: Int64, _ x: StructSeveralI64, j: Float) -> StructSeveralI64 {
    return StructSeveralI64(x1: x.x1, x2: x.x2 + i, x3: x.x3, x4: -x.x4, x5: x.x5 + Int64(j))
}

public func printStructSeveralI64(_ x: StructSeveralI64) {
    print("StructSeveralI64.1 = \(x.x1), .2 = \(x.x2), .3 = \(x.x3), .4 = \(x.x4), .5 = \(x.x5)")
}

// CHECK: SWIFT_EXTERN void $s7Structs27passThroughStructSeveralI641i_1jAA0deF0Vs5Int64V_AFSftF(SWIFT_INDIRECT_RESULT void * _Nonnull, int64_t i, const void * _Nonnull x, float j) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: SWIFT_EXTERN void $s7Structs21printStructSeveralI64yyAA0cdE0VF(const void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: SWIFT_EXTERN void $s7Structs25returnNewStructSeveralI641iAA0deF0Vs5Int64V_tF(SWIFT_INDIRECT_RESULT void * _Nonnull, int64_t i) SWIFT_NOEXCEPT SWIFT_CALL;
