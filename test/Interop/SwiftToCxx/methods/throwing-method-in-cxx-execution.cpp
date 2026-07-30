// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/throwing-method-in-cxx.swift -module-name Methods -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/methods.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-methods-errors-execution.o -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %S/throwing-method-in-cxx.swift -o %t/swift-methods-errors-execution -Xlinker %t/swift-methods-errors-execution.o -module-name Methods -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX

// RUN: %target-codesign %t/swift-methods-errors-execution
// RUN: %target-run %t/swift-methods-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc

// rdar://102167469
// UNSUPPORTED: CPU=arm64e

#include <cassert>
#include <cstdio>
#include <exception>
#include "methods.h"

int main() {
  using namespace Methods;

  auto value = ThrowingStruct::init(7);

  // Success paths.
  {
    swift::Int x = value.throwingMethod(false);
    printf("throwingMethod: %zd\n", static_cast<ptrdiff_t>(x));
    value.throwingVoidMethod(false);
    value.throwingMutatingMethod(false);
    swift::Int y = value.throwingMethod(false);
    printf("after mutation: %zd\n", static_cast<ptrdiff_t>(y));
    swift::Int s = ThrowingStruct::throwingStaticMethod(false);
    printf("static: %zd\n", static_cast<ptrdiff_t>(s));
    auto other = value.throwingStructReturn(false);
    printf("struct return: %zd\n",
           static_cast<ptrdiff_t>(other.throwingMethod(false)));
    auto large = value.throwingLargeStructReturn(false);
    printf("large struct return: %zd\n", static_cast<ptrdiff_t>(large.getE()));
  }

  // Error paths.
  try {
    (void)value.throwingMethod(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("throwingMethod threw");
  }
  try {
    value.throwingVoidMethod(true);
    puts("no exception");
  } catch (const std::exception &e) {
    printf("throwingVoidMethod threw: %s\n", e.what());
  }
  try {
    value.throwingMutatingMethod(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("throwingMutatingMethod threw");
  }
  {
    // The failed mutation must not have changed the value.
    swift::Int x = value.throwingMethod(false);
    printf("after failed mutation: %zd\n", static_cast<ptrdiff_t>(x));
  }
  try {
    (void)ThrowingStruct::throwingStaticMethod(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("throwingStaticMethod threw");
  }
  try {
    (void)value.throwingStructReturn(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("throwingStructReturn threw");
  }
  try {
    (void)value.throwingLargeStructReturn(true);
    puts("no exception");
  } catch (const swift::Error &e) {
    puts("throwingLargeStructReturn threw");
  }

  // Class methods.
  {
    auto object = ThrowingClass::init();
    swift::Int x = object.throwingClassMethod(false);
    printf("class method: %zd\n", static_cast<ptrdiff_t>(x));
    auto sameObject = object.throwingReturnsClass(false);
    printf("returned class: %zd\n",
           static_cast<ptrdiff_t>(sameObject.throwingClassMethod(false)));
    try {
      (void)object.throwingClassMethod(true);
      puts("no exception");
    } catch (const swift::Error &e) {
      puts("throwingClassMethod threw");
    }
    try {
      (void)object.throwingReturnsClass(true);
      puts("no exception");
    } catch (const swift::Error &e) {
      puts("throwingReturnsClass threw");
    }
  }
  return 0;
}

// CHECK: passThrowingMethod
// CHECK-NEXT: throwingMethod: 7
// CHECK-NEXT: passThrowingVoidMethod
// CHECK-NEXT: passThrowingMutatingMethod
// CHECK-NEXT: passThrowingMethod
// CHECK-NEXT: after mutation: 8
// CHECK-NEXT: passThrowingStaticMethod
// CHECK-NEXT: static: 99
// CHECK-NEXT: passThrowingStructReturn
// CHECK-NEXT: passThrowingMethod
// CHECK-NEXT: struct return: 108
// CHECK-NEXT: passThrowingLargeStructReturn
// CHECK-NEXT: large struct return: 5
// CHECK-NEXT: passThrowingMethod
// CHECK-NEXT: throwingMethod threw
// CHECK-NEXT: passThrowingVoidMethod
// CHECK-NEXT: throwingVoidMethod threw: failure
// CHECK-NEXT: passThrowingMutatingMethod
// CHECK-NEXT: throwingMutatingMethod threw
// CHECK-NEXT: passThrowingMethod
// CHECK-NEXT: after failed mutation: 8
// CHECK-NEXT: passThrowingStaticMethod
// CHECK-NEXT: throwingStaticMethod threw
// CHECK-NEXT: passThrowingStructReturn
// CHECK-NEXT: throwingStructReturn threw
// CHECK-NEXT: passThrowingLargeStructReturn
// CHECK-NEXT: throwingLargeStructReturn threw
// CHECK-NEXT: passThrowingClassMethod
// CHECK-NEXT: class method: 42
// CHECK-NEXT: passThrowingReturnsClass
// CHECK-NEXT: passThrowingClassMethod
// CHECK-NEXT: returned class: 42
// CHECK-NEXT: passThrowingClassMethod
// CHECK-NEXT: throwingClassMethod threw
// CHECK-NEXT: passThrowingReturnsClass
// CHECK-NEXT: throwingReturnsClass threw
