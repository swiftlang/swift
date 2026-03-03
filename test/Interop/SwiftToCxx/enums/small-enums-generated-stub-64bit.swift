// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: PTRSIZE=64

public enum Small {
    case first(Int)
    case second(Double)
}

public func passThroughSmall(_ en: Small) -> Small {
    return en
}

// CHECK:      struct swift_interop_returnStub_Enums_uint64_t_0_8_uint8_t_8_9 {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT:   uint8_t _2;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK void swift_interop_returnDirect_Enums_uint64_t_0_8_uint8_t_8_9(char * _Nonnull result, struct swift_interop_returnStub_Enums_uint64_t_0_8_uint8_t_8_9 value) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:   memcpy(result + 8, &value._2, 1);
// CHECK-NEXT: }

// CHECK:      struct swift_interop_passStub_Enums_uint64_t_0_8_uint8_t_8_9 {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT:   uint8_t _2;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Enums_uint64_t_0_8_uint8_t_8_9 swift_interop_passDirect_Enums_uint64_t_0_8_uint8_t_8_9(const char * _Nonnull value) {
// CHECK-NEXT:   struct swift_interop_passStub_Enums_uint64_t_0_8_uint8_t_8_9 result;
// CHECK-NEXT:   memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:   memcpy(&result._2, value + 8, 1);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
