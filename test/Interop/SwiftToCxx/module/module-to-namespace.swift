// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -module-name Test -emit-cxx-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-interop-cxx-header-in-clang(%t/empty.h)

// CHECK-LABEL: namespace Test {
// CHECK:       } // namespace Test
