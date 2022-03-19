// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Test -enable-cxx-interop -emit-clang-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-interop-cxx-header-in-clang(%t/empty.h)

// CHECK-LABEL: namespace Test {
// CHECK:       } // namespace Test
