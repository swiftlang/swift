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

  auto accessors = Functions::ThrowingAccessors::init(false);
  printf("value: %d\n", static_cast<int>(accessors.getValue()));
  printf("isReady: %d\n", static_cast<int>(accessors.isReady()));
  printf("subscript: %d\n", static_cast<int>(accessors[4]));
  auto failingAccessors = Functions::ThrowingAccessors::init(true);
  try {
    (void)failingAccessors.getValue();
  } catch (swift::Error &e) {
    printf("Getter exception\n");
  }
  try {
    (void)failingAccessors[4];
  } catch (swift::Error &e) {
    printf("Subscript exception\n");
  }
  try {
    (void)Functions::ThrowingAccessors::getStaticValue();
  } catch (swift::Error &e) {
    auto errorOpt = e.as<Functions::NaiveErrors>();
    assert(errorOpt.isSome());
    assert(errorOpt.get() == Functions::NaiveErrors::returnError);
    printf("Static getter exception\n");
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
// CHECK-NEXT: value: 42
// CHECK-NEXT: isReady: 1
// CHECK-NEXT: subscript: 8
// CHECK-NEXT: Getter exception
// CHECK-NEXT: Subscript exception
// CHECK-NEXT: Static getter exception
