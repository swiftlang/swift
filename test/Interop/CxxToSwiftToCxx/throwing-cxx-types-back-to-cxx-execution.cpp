// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}use-cxx-types.swift -module-name UseCxx -typecheck -verify \
// RUN:   -emit-clang-header-path %t%{fs-sep}UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %FileCheck %s --input-file %t%{fs-sep}UseCxx.h

// RUN: %target-interop-build-clangxx -std=c++20 -c %t%{fs-sep}use-swift-cxx-types.cpp -I %t -o %t%{fs-sep}exceptions.o \
// RUN:   -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}exceptions -Xlinker %t%{fs-sep}exceptions.o \
// RUN:   -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %target-codesign %t%{fs-sep}exceptions
// RUN: %target-run %t%{fs-sep}exceptions

// RUN: %target-interop-build-clangxx -std=c++20 -fno-exceptions -c %t%{fs-sep}use-swift-cxx-types.cpp -I %t -o %t%{fs-sep}no-exceptions.o \
// RUN:   -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}no-exceptions -Xlinker %t%{fs-sep}no-exceptions.o \
// RUN:   -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %target-codesign %t%{fs-sep}no-exceptions
// RUN: %target-run %t%{fs-sep}no-exceptions

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: CPU=arm64e

//--- header.h

inline int livingNonTrivial = 0;

struct Trivial {
  int x, y;
};

struct NonTrivial {
  int value;
  NonTrivial(int value) : value(value) { ++livingNonTrivial; }
  NonTrivial(const NonTrivial &other) : value(other.value) {
    ++livingNonTrivial;
  }
  NonTrivial(NonTrivial &&other) : value(other.value) { ++livingNonTrivial; }
  ~NonTrivial() { --livingNonTrivial; }
};

struct ImmortalFRT {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

inline ImmortalFRT *getImmortalFRT() {
  static ImmortalFRT frt{42};
  return &frt;
}

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

enum CxxResultError: Error { case failure }

func check(_ fail: Bool) throws {
  if fail { throw CxxResultError.failure }
}

public func throwingTrivial(_ fail: Bool) throws -> Trivial {
  try check(fail)
  return Trivial(x: 1, y: 2)
}

public func throwingNonTrivial(_ fail: Bool) throws -> NonTrivial {
  try check(fail)
  return NonTrivial(42)
}

// Using `NonTrivial` as a generic argument also makes sure that its type
// metadata, which the call to `throwingGeneric` from C++ needs, is emitted.
public func throwingOptionalNonTrivial(_ fail: Bool) throws -> NonTrivial? {
  try check(fail)
  return NonTrivial(42)
}

public func throwingFRT(_ fail: Bool) throws -> ImmortalFRT {
  try check(fail)
  return getImmortalFRT()
}

public func throwingGeneric<T>(_ value: T, _ fail: Bool) throws -> T {
  try check(fail)
  return value
}

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <assert.h>

template <class Operation, class Validate>
void checkResult(Operation operation, Validate validate) {
#ifdef __cpp_exceptions
  validate(operation(false));
  try {
    (void)operation(true);
    assert(false && "the Swift error must propagate");
  } catch (const swift::Error &) {
  }
#else
  auto value = operation(false);
  assert(value.has_value());
  validate(value.value());
  assert(!operation(true).has_value());
#endif
}

int main() {
  checkResult(UseCxx::throwingTrivial, [](const Trivial &value) {
    assert(value.x == 1 && value.y == 2);
  });
  checkResult(UseCxx::throwingNonTrivial, [](const NonTrivial &value) {
    assert(value.value == 42);
  });
  checkResult(UseCxx::throwingOptionalNonTrivial,
              [](swift::Optional<NonTrivial> value) {
                assert(value.isSome() && value.get().value == 42);
              });
  assert(livingNonTrivial == 0);
  checkResult(UseCxx::throwingFRT,
              [](ImmortalFRT *value) { assert(value->value == 42); });

  NonTrivial nonTrivial(42);
  checkResult(
      [&](bool fail) { return UseCxx::throwingGeneric(nonTrivial, fail); },
      [](const NonTrivial &value) { assert(value.value == 42); });
  assert(livingNonTrivial == 1);
  return 0;
}

// The result is only read from its storage after the error check.
// CHECK-LABEL: swift::ThrowingResult<ImmortalFRT *_Nonnull> throwingFRT(bool fail)
// CHECK: auto returnValue = UseCxx::_impl::$s6UseCxx11throwingFRTySo08ImmortalD0VSbKF(fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return SWIFT_RETURN_THUNK(ImmortalFRT *_Nonnull, returnValue);

// CHECK-LABEL: swift::ThrowingResult<T_0_0> throwingGeneric(const T_0_0& value, bool fail)
// CHECK: } else if constexpr (::swift::_impl::isSwiftBridgedCxxRecord<T_0_0>) {
// CHECK-NEXT: alignas(alignof(T_0_0)) char storage[sizeof(T_0_0)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<T_0_0 *>(storage);
// CHECK-NEXT: UseCxx::_impl::$s6UseCxx15throwingGenericyxx_SbtKlF(storage, {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: T_0_0 result(static_cast<T_0_0 &&>(*storageObjectPtr));

// CHECK-LABEL: swift::ThrowingResult<NonTrivial> throwingNonTrivial(bool fail)
// CHECK: alignas(alignof(NonTrivial)) char storage[sizeof(NonTrivial)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<NonTrivial *>(storage);
// CHECK-NEXT: UseCxx::_impl::$s6UseCxx18throwingNonTrivialySo0dE0VSbKF(storage, fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<NonTrivial>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: NonTrivial result(static_cast<NonTrivial &&>(*storageObjectPtr));

// CHECK-LABEL: swift::ThrowingResult<Trivial> throwingTrivial(bool fail)
// CHECK: UseCxx::_impl::swift_interop_returnDirect_UseCxx_{{.*}}(storage, UseCxx::_impl::$s6UseCxx15throwingTrivialySo0D0VSbKF(fail, _ctx, &opaqueError));
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return *storageObjectPtr;
