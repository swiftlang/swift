// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/swift-error-exception.swift -module-name
// ErrorException -clang-header-expose-decls=all-public
// -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// -typecheck -emit-clang-header-path %t/error.h RUN:
// %target-interop-build-clangxx -c %s -I %t -o %t/test.o
// -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR RUN: %target-interop-build-swift
// %S/swift-error-exception.swift -o %t/test -Xlinker %t/test.o -module-name
// ErrorException -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: CPU=arm64e

#include "error.h"
#include <cassert>
#include <cstring>
#include <exception>
#include <type_traits>
#include <utility>

static_assert(std::is_base_of<std::exception, swift::Error>::value, "");
static_assert(noexcept(std::declval<const swift::Error &>().what()), "");

int main() {
  swift::Error empty;
  assert(std::strcmp(empty.what(), "swift::Error: no error value") == 0);
  try {
    ErrorException::failSimply();
    assert(false);
  } catch (const std::exception &error) {
    assert(std::strcmp(error.what(), "failure") == 0);
  }
  try {
    ErrorException::failDescriptively();
    assert(false);
  } catch (const swift::Error &error) {
    const char *text = error.what();
    assert(std::strcmp(text, "custom error: café ☕") == 0);
    assert(text == error.what());
    assert(error.as<ErrorException::DescriptiveError>().get().getCode() == 7);

    swift::Error copy(error);
    assert(std::strcmp(copy.what(), text) == 0);
    swift::Error assigned;
    (void)assigned.what();
    assigned = copy;
    assert(std::strcmp(assigned.what(), text) == 0);
    swift::Error moved(std::move(copy));
    assert(std::strcmp(moved.what(), text) == 0);
    assigned = std::move(moved);
    assert(std::strcmp(assigned.what(), text) == 0);
  }
}
