// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -module-name Test -emit-cxx-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-cxx-header-in-clang -std=c++14 %t/empty.h
// RUN: %check-cxx-header-in-clang -std=c++17 %t/empty.h

// CHECK-LABEL: namespace Test {
// CHECK:       } // namespace Test
