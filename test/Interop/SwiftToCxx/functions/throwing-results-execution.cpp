// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/throwing-results.swift -module-name Results
// -clang-header-expose-decls=all-public -enable-experimental-feature
// GenerateBindingsForThrowingFunctionsInCXX -typecheck -emit-clang-header-path
// %t/results.h RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o
// %t/test.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR RUN:
// %target-interop-build-swift %S/throwing-results.swift -o %t/test -Xlinker
// %t/test.o -module-name Results -Xfrontend -entry-point-function-name
// -Xfrontend swiftMain RUN: %target-codesign %t/test RUN: %target-run %t/test
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %s -I %t -o
// %t/no-exceptions.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR RUN:
// %target-interop-build-swift %S/throwing-results.swift -o %t/no-exceptions
// -Xlinker %t/no-exceptions.o -module-name Results -Xfrontend
// -entry-point-function-name -Xfrontend swiftMain RUN: %target-codesign
// %t/no-exceptions RUN: %target-run %t/no-exceptions REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: CPU=arm64e

#include "results.h"
#include <cassert>
#include <string>

template <class Operation, class Validate>
void checkResult(Operation operation, Validate validate) {
#ifdef __cpp_exceptions
  {
    auto value = operation(false);
    validate(value);
  }
  try {
    (void)operation(true);
    assert(false && "a Swift error must propagate");
  } catch (swift::Error &error) {
    assert(error.as<Results::ResultError>().get() ==
           Results::ResultError::failure);
  }
#else
  {
    auto value = operation(false);
    assert(value.has_value());
    validate(value.value());
  }
  auto error = operation(true);
  assert(!error.has_value());
  assert(error.error().template as<Results::ResultError>().get() ==
         Results::ResultError::failure);
#endif
}

int main() {
  auto small = [](const Results::Small &value) {
    assert(value.getValue() == 42);
  };
  auto large = [](const Results::Large &value) {
    assert(value.getA() == 1 && value.getE() == 5);
    assert(Results::canaryCount() == 1);
  };
  auto ref = [](Results::Ref &value) { assert(value.getValue() == 42); };
  checkResult(Results::direct, small);
  checkResult(Results::indirect, large);
  assert(Results::canaryCount() == 0);
  checkResult(Results::reference, ref);
  checkResult(Results::string, [](const swift::String &value) {
    assert(std::string(value) == "Hello from Swift");
  });
  checkResult([](bool fail) { return Results::Small::init(42, fail); }, small);
  checkResult(Results::Large::init, large);
  assert(Results::canaryCount() == 0);
  checkResult(Results::Holder::init, [](const Results::Holder &) {
    assert(Results::canaryCount() == 1);
  });
  assert(Results::canaryCount() == 0);
  checkResult(Results::Ref::init, ref);
  checkResult(Results::Small::make, small);
  auto value = Results::Small::init(21);
  checkResult([&](bool fail) { return value.doubled(fail); }, small);
  checkResult([&](bool fail) { return value.increment(fail); },
              [](swift::Int i) { assert(i == 22); });
  assert(value.getValue() == 22);
  checkResult([](bool fail) { return Results::generic(swift::Int(42), fail); },
              [](swift::Int i) { assert(i == 42); });
  auto genericValue = Results::Small::init(42);
  checkResult([&](bool fail) { return Results::generic(genericValue, fail); },
              small);
  // Exercise all generic return representations, including reference values.
  checkResult(Results::reference, [&](Results::Ref &object) {
    checkResult([&](bool fail) { return Results::generic(object, fail); }, ref);
    checkResult([&](bool fail) { return object.result(fail); }, small);
  });
  checkResult(Results::indirect, [&](const Results::Large &object) {
    checkResult([&](bool fail) { return Results::generic(object, fail); },
                large);
  });
  assert(Results::canaryCount() == 0);
}
