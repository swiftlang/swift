// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/throwing-void.swift -module-name ThrowingVoid
// -clang-header-expose-decls=all-public -enable-experimental-feature
// GenerateBindingsForThrowingFunctionsInCXX -typecheck -emit-clang-header-path
// %t/void.h RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %s
// -I %t -o %t/test.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR RUN:
// %target-interop-build-swift %S/throwing-void.swift -o %t/test -Xlinker
// %t/test.o -module-name ThrowingVoid -Xfrontend -entry-point-function-name
// -Xfrontend swiftMain RUN: %target-codesign %t/test RUN: %target-run %t/test
// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: CPU=arm64e

#include "void.h"
#include <cassert>

int main() {
  swift::Expected<void> success;
  assert(success.has_value());
  const auto copy = success;
  assert(copy.has_value());
  swift::Error error;
  swift::Expected<void> failure(error);
  assert(!failure.has_value());
  const auto errorCopy = failure;
  assert(!errorCopy.has_value());

  assert(ThrowingVoid::checkedVoid(false).has_value());
  assert(!ThrowingVoid::checkedVoid(true).has_value());
  assert(ThrowingVoid::genericVoid(swift::Int(1), false).has_value());
  assert(!ThrowingVoid::genericVoid(swift::Int(1), true).has_value());
  assert(!ThrowingVoid::genericNever(swift::Int(1)).has_value());
  auto object = ThrowingVoid::VoidMethods::init();
  assert(object.checked(false).has_value());
  assert(!object.checked(true).has_value());
}
