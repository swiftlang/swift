// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-functions-errors.swift -module-name Functions -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr-or-stdlib -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-errors-execution.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %S/swift-functions-errors.swift -o %t/swift-functions-errors-execution -Xlinker %t/swift-functions-errors-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX

// RUN: %target-codesign %t/swift-functions-errors-execution
// RUN: %target-run %t/swift-functions-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc

// rdar://102167469
// UNSUPPORTED: CPU=arm64e

#include <cassert>
#include <cstdio>
#include <string>
#include "functions.h"

int main() {
  static_assert(!noexcept(Functions::emptyThrowFunction()), "noexcept function");
  static_assert(!noexcept(Functions::throwFunction()), "noexcept function");
  static_assert(!noexcept(Functions::throwFunctionWithReturn()), "noexcept function");

  try {
    Functions::emptyThrowFunction();
  } catch (swift::Error& e) {
    printf("Exception\n");
  }
  try {
    Functions::throwFunction();
  } catch (swift::Error& e) {
      auto errorOpt = e.as<Functions::NaiveErrors>();
      assert(errorOpt.isSome());

      auto errorVal = errorOpt.get();
      assert(errorVal == Functions::NaiveErrors::throwError);
      errorVal.getMessage();
  }
  try {
    Functions::throwFunctionWithReturn();
  } catch (swift::Error& e) {
     printf("Exception\n");
  }
  try {
    Functions::throwFunctionWithNeverReturn();
  } catch (swift::Error& e) {
     printf("Exception\n");
  }
  try {
    Functions::testDestroyedError();
  } catch(const swift::Error &e) { }

  // Throwing functions returning a struct with direct/indirect returns and
  // a String.
  {
    auto smallResult = Functions::throwFunctionWithDirectStructReturn(false);
    printf("direct struct: %zd\n",
           static_cast<ptrdiff_t>(smallResult.getValue()));
    auto largeResult = Functions::throwFunctionWithIndirectStructReturn(false);
    printf("indirect struct: %zd %zd\n",
           static_cast<ptrdiff_t>(largeResult.getA()),
           static_cast<ptrdiff_t>(largeResult.getE()));
    std::string stringResult =
        Functions::throwFunctionWithStringReturn(false);
    printf("string: %s\n", stringResult.c_str());
  }
  try {
    (void)Functions::throwFunctionWithDirectStructReturn(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("direct struct threw");
  }
  try {
    (void)Functions::throwFunctionWithIndirectStructReturn(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("indirect struct threw");
  }
  try {
    (void)Functions::throwFunctionWithStringReturn(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("string threw");
  }

  // Generic throwing functions.
  try {
    Functions::genericThrowFunction(swift::Int(7));
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("generic threw");
  }
  {
    swift::Int genericValue =
        Functions::genericThrowFunctionWithReturn(swift::Int(11), false);
    printf("generic primitive: %zd\n", static_cast<ptrdiff_t>(genericValue));
    auto genericStruct = Functions::genericThrowFunctionWithReturn(
        Functions::SmallResult::init(5), false);
    printf("generic struct: %zd\n",
           static_cast<ptrdiff_t>(genericStruct.getValue()));
  }
  try {
    (void)Functions::genericThrowFunctionWithReturn(swift::Int(11), true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("generic primitive threw");
  }
  try {
    (void)Functions::genericThrowFunctionWithReturn(
        Functions::SmallResult::init(5), true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("generic struct threw");
  }

  return 0;
}

// CHECK: passEmptyThrowFunction
// CHECK-NEXT: passThrowFunction
// CHECK-NEXT: throwError
// CHECK-NEXT: passThrowFunctionWithReturn
// CHECK-NEXT: Exception
// CHECK-NEXT: passThrowFunctionWithNeverReturn
// CHECK-NEXT: Exception
// CHECK-NEXT: Test destroyed
// CHECK-NEXT: passThrowFunctionWithDirectStructReturn
// CHECK-NEXT: direct struct: 42
// CHECK-NEXT: passThrowFunctionWithIndirectStructReturn
// CHECK-NEXT: indirect struct: 1 5
// CHECK-NEXT: passThrowFunctionWithStringReturn
// CHECK-NEXT: string: Hello from Swift
// CHECK-NEXT: passThrowFunctionWithDirectStructReturn
// CHECK-NEXT: direct struct threw
// CHECK-NEXT: passThrowFunctionWithIndirectStructReturn
// CHECK-NEXT: indirect struct threw
// CHECK-NEXT: passThrowFunctionWithStringReturn
// CHECK-NEXT: string threw
// CHECK-NEXT: passGenericThrowFunction
// CHECK-NEXT: generic threw
// CHECK-NEXT: passGenericThrowFunctionWithReturn
// CHECK-NEXT: generic primitive: 11
// CHECK-NEXT: passGenericThrowFunctionWithReturn
// CHECK-NEXT: generic struct: 5
// CHECK-NEXT: passGenericThrowFunctionWithReturn
// CHECK-NEXT: generic primitive threw
// CHECK-NEXT: passGenericThrowFunctionWithReturn
// CHECK-NEXT: generic struct threw
