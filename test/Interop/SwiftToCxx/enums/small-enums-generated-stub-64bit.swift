// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

// REQUIRES: PTRSIZE=64

public enum Small {
    case first(Int)
    case second(Double)
}

public func passThroughSmall(_ en: Small) -> Small {
    return en
}

// CHECK:      struct swift_interop_stub_Enums_Small {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT:   uint8_t _2;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Enums_Small(char * _Nonnull result, struct swift_interop_stub_Enums_Small value) __attribute__((always_inline)) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:   memcpy(result + 8, &value._2, 1);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Enums_Small swift_interop_passDirect_Enums_Small(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:   struct swift_interop_stub_Enums_Small result;
// CHECK-NEXT:   memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:   memcpy(&result._2, value + 8, 1);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
