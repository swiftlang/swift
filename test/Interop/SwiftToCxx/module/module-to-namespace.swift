// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Test -clang-header-expose-decls=all-public -emit-clang-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-interop-cxx-header-in-clang(%t/empty.h)

// CHECK-LABEL: namespace Test __attribute__((swift_private)) {
// CHECK:       } // namespace Test
