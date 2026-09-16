// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Guards -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/guards.h
// RUN: %FileCheck %s < %t/guards.h
// RUN: %check-interop-cxx-header-in-clang(%t/guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)
// RUN: %target-interop-build-clangxx -fsyntax-only %S/Inputs/throwing-bindings-guards.cpp -I %t
// RUN: %target-interop-build-clangxx -fsyntax-only %S/Inputs/throwing-bindings-guards.cpp -I %t -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public func ordinary() -> Int { 42 }
public func throwing() throws -> Int { 42 }

public struct Guarded {
  public var value: Int
  public init(_ value: Int) { self.value = value }
  public init(checked value: Int) throws { self.value = value }
  public func checked() throws -> Int { value }
  public static func checkedStatic() throws -> Int { 42 }
}

// The low-level declaration, member declaration and out-of-line thunk must
// all use the same opt-in condition as swift::Error/ThrowingResult.
// CHECK: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: SWIFT_EXTERN
// CHECK: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR)
// CHECK: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> checked()
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR)
// CHECK: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> Guarded::checked()
