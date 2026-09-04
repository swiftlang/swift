// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/throwing-getter-in-cxx.swift -module-name Properties -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/properties.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-properties-errors-execution.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %S/throwing-getter-in-cxx.swift -o %t/swift-properties-errors-execution -Xlinker %t/swift-properties-errors-execution.o -module-name Properties -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX

// RUN: %target-codesign %t/swift-properties-errors-execution
// RUN: %target-run %t/swift-properties-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc

// rdar://102167469
// UNSUPPORTED: CPU=arm64e

#include <cassert>
#include <cstdio>
#include <exception>
#include "properties.h"

int main() {
  using namespace Properties;

  auto props = ThrowingProps::init(false);
  swift::Int value = props.getComputed();
  printf("computed: %zd\n", static_cast<ptrdiff_t>(value));
  swift::Int element = props[4];
  printf("subscript: %zd\n", static_cast<ptrdiff_t>(element));

  auto throwingProps = ThrowingProps::init(true);
  try {
    (void)throwingProps.getComputed();
    puts("no exception");
  } catch (const std::exception &e) {
    printf("getter threw: %s\n", e.what());
  }
  try {
    (void)throwingProps[4];
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("subscript threw");
  }
  return 0;
}

// CHECK: passThrowingGetter
// CHECK-NEXT: computed: 21
// CHECK-NEXT: passThrowingSubscript
// CHECK-NEXT: subscript: 8
// CHECK-NEXT: passThrowingGetter
// CHECK-NEXT: getter threw: failure
// CHECK-NEXT: passThrowingSubscript
// CHECK-NEXT: subscript threw
