// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Test -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-interop-cxx-header-in-clang(%t/empty.h)

// CHECK: #ifdef SWIFT_SYMBOL
// CHECK-NEXT: #undef SWIFT_SYMBOL
// CHECK-NEXT: #endif
// CHECK-NEXT: #define SWIFT_SYMBOL(usrValue) SWIFT_SYMBOL_MODULE_USR("Test", usrValue)

// CHECK-LABEL: namespace Test SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE({{.*}}) {
// CHECK:       } // namespace Test
// CHECK-EMPTY:
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: #undef SWIFT_SYMBOL
