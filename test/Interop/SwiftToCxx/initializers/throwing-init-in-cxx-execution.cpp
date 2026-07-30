// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/throwing-init-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/inits.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-inits-errors-execution.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %S/throwing-init-in-cxx.swift -o %t/swift-inits-errors-execution -Xlinker %t/swift-inits-errors-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX

// RUN: %target-codesign %t/swift-inits-errors-execution
// RUN: %target-run %t/swift-inits-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc

// rdar://102167469
// UNSUPPORTED: CPU=arm64e

#include <cassert>
#include <cstdio>
#include <exception>
#include "inits.h"

int main() {
  using namespace Init;

  // Success paths.
  {
    auto smallStruct = StructWithThrowingInit::init(11);
    printf("small struct: %zd\n",
           static_cast<ptrdiff_t>(smallStruct.getValue()));
    auto refHolder = RefHolderWithThrowingInit::init(false);
    puts("ref holder initialized");
    auto largeStruct = LargeStructWithThrowingInit::init(20);
    printf("large struct: %zd %zd\n",
           static_cast<ptrdiff_t>(largeStruct.getA()),
           static_cast<ptrdiff_t>(largeStruct.getE()));
    auto object = ClassWithThrowingInit::init(30);
    printf("class: %zd\n", static_cast<ptrdiff_t>(object.getValue()));
  }
  puts("success scope ended");

  // Error paths. The thrown error must propagate and no value may be
  // materialized (or destroyed) on the C++ side.
  try {
    (void)StructWithThrowingInit::init(-1);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("struct init threw");
  }
  try {
    (void)RefHolderWithThrowingInit::init(true);
    puts("no exception");
  } catch (const std::exception &e) {
    printf("ref holder init threw: %s\n", e.what());
  }
  try {
    (void)LargeStructWithThrowingInit::init(-1);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("large struct init threw");
  }
  try {
    (void)ClassWithThrowingInit::init(-1);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("class init threw");
  }
  puts("done");
  return 0;
}

// CHECK: passStructThrowingInit
// CHECK-NEXT: small struct: 11
// CHECK-NEXT: passRefHolderThrowingInit
// CHECK-NEXT: ref holder initialized
// CHECK-NEXT: passLargeStructThrowingInit
// CHECK-NEXT: large struct: 20 24
// CHECK-NEXT: passClassThrowingInit
// CHECK-NEXT: class: 30
// CHECK-NEXT: ClassWithThrowingInit destroyed
// CHECK-NEXT: Canary destroyed
// CHECK-NEXT: success scope ended
// CHECK-NEXT: passStructThrowingInit
// CHECK-NEXT: struct init threw
// CHECK-NEXT: passRefHolderThrowingInit
// CHECK-NEXT: ref holder init threw: failure
// CHECK-NEXT: passLargeStructThrowingInit
// CHECK-NEXT: large struct init threw
// CHECK-NEXT: passClassThrowingInit
// CHECK-NEXT: class init threw
// CHECK-NEXT: done
