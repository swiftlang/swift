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

template <class Operation, class Validate>
void checkResult(Operation operation, Validate validate) {
  validate(operation(false));
  try {
    (void)operation(true);
    assert(false && "the Swift error must propagate");
  } catch (swift::Error &e) {
    assert(e.as<Functions::NaiveErrors>().get() ==
           Functions::NaiveErrors::throwError);
  }
}

void checkResults() {
  auto small = [](const Functions::SmallResult &value) {
    assert(value.getValue() == 42);
  };
  auto large = [](const Functions::LargeResult &value) {
    assert(value.getA() == 1 && value.getE() == 5);
    assert(Functions::livingCanaries() == 1);
  };
  auto ref = [](Functions::RefResult value) { assert(value.getValue() == 42); };
  checkResult(Functions::throwingSmall, small);
  checkResult(Functions::throwingLarge, large);
  checkResult(Functions::throwingOptionalLarge,
              [&](swift::Optional<Functions::LargeResult> value) {
                assert(value.isSome());
                large(value.get());
              });
  checkResult(Functions::throwingRef, ref);
  checkResult(Functions::throwingString, [](const swift::String &value) {
    assert(std::string(value) == "Hello");
  });
  checkResult([](bool fail) { return Functions::SmallResult::init(42, fail); },
              small);
  checkResult(Functions::SmallResult::make, small);
  checkResult(Functions::LargeResult::init, large);
  checkResult(Functions::RefResult::init, ref);
  assert(Functions::livingCanaries() == 0);

  auto smallValue = Functions::SmallResult::init(21, false);
  checkResult([&](bool fail) { return smallValue.doubled(fail); }, small);
  auto refValue = Functions::RefResult::init(false);
  checkResult([&](bool fail) { return refValue.small(fail); }, small);

  checkResult(
      [](bool fail) {
        return Functions::throwingGeneric(swift::Int(42), fail);
      },
      [](swift::Int value) { assert(value == 42); });
  checkResult(
      [&](bool fail) { return Functions::throwingGeneric(refValue, fail); },
      ref);
  auto largeValue = Functions::LargeResult::init(false);
  checkResult(
      [&](bool fail) { return Functions::throwingGeneric(largeValue, fail); },
      large);
}

int main() {
  checkResults();
  assert(Functions::livingCanaries() == 0);

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
