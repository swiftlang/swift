// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-functions-errors.swift -module-name Functions -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr-or-stdlib -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-errors-execution.o -pthread -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %S/swift-functions-errors.swift -o %t/swift-functions-errors-execution -Xlinker %t/swift-functions-errors-execution.o -Xclang-linker -pthread -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX

// RUN: %target-codesign %t/swift-functions-errors-execution
// RUN: %target-run %t/swift-functions-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc

// rdar://102167469
// UNSUPPORTED: CPU=arm64e

#include "functions.h"
#include <atomic>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <thread>
#include <utility>

int main() {
  static_assert(!noexcept(Functions::emptyThrowFunction()), "noexcept function");
  static_assert(!noexcept(Functions::throwFunction()), "noexcept function");
  static_assert(!noexcept(Functions::throwFunctionWithReturn()), "noexcept function");

  // Concurrent first calls must return the same cached description.
  try {
    Functions::throwDescriptiveError();
  } catch (const swift::Error &e) {
    std::atomic<bool> start{false};
    const char *descriptions[4] = {};
    std::thread readers[4];
    for (int i = 0; i < 4; ++i) {
      readers[i] = std::thread([&, i] {
        while (!start.load(std::memory_order_acquire))
          std::this_thread::yield();
        descriptions[i] = e.what();
      });
    }
    start.store(true, std::memory_order_release);
    for (auto &reader : readers)
      reader.join();
    for (const char *description : descriptions) {
      assert(description == descriptions[0]);
      assert(std::strcmp(description, "custom error: café ☕") == 0);
    }
  }

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
  } catch (const swift::Error &e) {
    // Describing and copying the error must still destroy its payload once.
    assert(e.what()[0] != '\0');
    swift::Error copy(e);
    assert(copy.what()[0] != '\0');
    swift::Error assigned;
    assigned = copy;
    assigned = std::move(copy);
  }

  static_assert(std::is_base_of<std::exception, swift::Error>::value,
                "swift::Error is not a std::exception");
  static_assert(noexcept(std::declval<const swift::Error &>().what()),
                "what() is not noexcept");

  swift::Error empty;
  printf("empty: %s\n", empty.what());

  swift::Error naive;
  try {
    Functions::throwFunction();
  } catch (const std::exception &e) {
    printf("what: %s\n", e.what());
    naive = static_cast<const swift::Error &>(e);
  }
  printf("copy-assigned: %s\n", naive.what());

  try {
    Functions::throwDescriptiveError();
  } catch (const swift::Error &e) {
    const char *description = e.what();
    printf("what: %s\n", description);
    assert(description == e.what());
    assert(e.as<Functions::DescriptiveError>().get().getCode() == 7);

    swift::Error copy(e);
    printf("copy: %s\n", copy.what());
    swift::Error moved(std::move(copy));
    printf("moved: %s\n", moved.what());
    printf("moved-from: %s\n", copy.what());

    swift::Error assigned(naive);
    printf("assigned: %s\n", assigned.what());
    assigned = std::move(moved);
    printf("move-assigned: %s\n", assigned.what());
    printf("moved-from: %s\n", moved.what());
    assigned = naive;
    printf("copy-assigned: %s\n", assigned.what());
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
// CHECK-NEXT: empty: swift::Error: no error value
// CHECK-NEXT: passThrowFunction
// CHECK-NEXT: what: throwError
// CHECK-NEXT: copy-assigned: throwError
// CHECK-NEXT: what: custom error: café ☕
// CHECK-NEXT: copy: custom error: café ☕
// CHECK-NEXT: moved: custom error: café ☕
// CHECK-NEXT: moved-from: swift::Error: no error value
// CHECK-NEXT: assigned: throwError
// CHECK-NEXT: move-assigned: custom error: café ☕
// CHECK-NEXT: moved-from: swift::Error: no error value
// CHECK-NEXT: copy-assigned: throwError
