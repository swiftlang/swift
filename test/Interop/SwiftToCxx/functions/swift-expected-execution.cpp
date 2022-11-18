// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-functions-errors.swift -typecheck -module-name Functions -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-expected-execution.o
// RUN: %target-interop-build-swift %S/swift-functions-errors.swift -o %t/swift-expected-execution -Xlinker %t/swift-expected-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-expected-execution
// RUN: %target-run %t/swift-expected-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cstdio>
#include "functions.h"

int main() {

  // Test Empty Constructor
  auto testIntEmpty = swift::Expected<int>();

  // Test Error Constructor
  swift::Error e;
  auto testIntError = swift::Expected<int>(e);

  // Test Value Constructor
  auto testIntValue = swift::Expected<int>(42);

  // Test Copy Constructor
  auto testCopy = testIntEmpty;

  // TODO: Test Move Constructor

  // Test Destructor
  auto testDestEmpty = swift::Expected<int>();
  auto testDestInt = swift::Expected<int>(42);
  auto testDestError = swift::Expected<int>(e);
  testDestEmpty.~Expected();
  testDestInt.~Expected();
  testDestError.~Expected();

  // TODO: Test Assignment (Move)

  // Test Access to T's members (const)
  const auto exp = testIntValue;
  if (*exp.operator->() == 42)
    printf("Test Access to T's members (const)\n");

  // Test Access to T's members
  if (*testIntValue.operator->() == 42)
    printf("Test Access to T's members\n");

  // Test Reference to T's members (const)
  const auto refExp = testIntValue;
  if (*refExp == 42)
    printf("Test Reference to T's members (const)\n");

  // Test Reference to T's members
  if (*testIntValue == 42)
    printf("Test Reference to T's members\n");

  // Test bool operator
  if (testIntValue) {
    printf("Test operator bool\n");
  }

  // Test get T's Value (const)
  const auto valueExp = testIntValue;
  if (valueExp.value() == 42)
    printf("Test get T's Value (const)\n");

  // Test get T's Value
  if (testIntValue.value() == 42)
    printf("Test get T's Value\n");

  // Test get swift::Error (const)
  try {
    Functions::throwFunction();
  } catch (swift::Error& e) {
    const auto errorExp = swift::Expected<int>(e);
    auto err = errorExp.error(); // using const function
    auto errorVal = err.as<Functions::NaiveErrors>();
    errorVal.getMessage();
  }

  // Test get swift::Error
  try {
    Functions::throwFunction();
  } catch (swift::Error& e) {
    auto err = swift::Expected<int>(e).error();
    auto errorVal = err.as<Functions::NaiveErrors>();
    errorVal.getMessage();
  }

  // Test has Value
  if (testIntValue.has_value())
    printf("testIntValue has a value\n");
  if (!testIntError.has_value())
    printf("testIntError doesn't have a value\n");

  return 0;
}

// CHECK: Test Access to T's members (const)
// CHECK-NEXT: Test Access to T's members
// CHECK-NEXT: Test Reference to T's members (const)
// CHECK-NEXT: Test Reference to T's members
// CHECK-NEXT: Test operator bool
// CHECK-NEXT: Test get T's Value (const)
// CHECK-NEXT: Test get T's Value
// CHECK-NEXT: passThrowFunction
// CHECK-NEXT: throwError
// CHECK-NEXT: passThrowFunction
// CHECK-NEXT: throwError
// CHECK-NEXT: testIntValue has a value
// CHECK-NEXT: testIntError doesn't have a value
