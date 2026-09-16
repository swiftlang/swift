// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name ThrowingVoid -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/void.h
// RUN: %FileCheck %s < %t/void.h
// RUN: %check-interop-cxx-header-in-clang(%t/void.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum VoidError: Error { case failure }

public func checkedVoid(_ fail: Bool) throws {
  if fail { throw VoidError.failure }
}

public func genericVoid<T>(_ value: T, _ fail: Bool) throws {
  try checkedVoid(fail)
}

public func genericNever<T>(_ value: T) throws -> Never {
  throw VoidError.failure
}

public final class VoidMethods {
  public init() {}
  public func checked(_ fail: Bool) throws { try checkedVoid(fail) }
}

// A generic Void result must not cast a GenericFunctionType to FunctionType.
// CHECK: swift::ThrowingResult<void> genericNever
// CHECK: abort();
// CHECK: swift::ThrowingResult<void> genericVoid
// CHECK: #ifndef __cpp_exceptions
// CHECK-NEXT: return swift::Expected<void>();

// Class self supplies the context; don't emit an unused placeholder.
// CHECK: swift::ThrowingResult<void> VoidMethods::checked
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NOT: void* _ctx
// CHECK: if (opaqueError != nullptr)
// CHECK: return swift::Expected<void>();
