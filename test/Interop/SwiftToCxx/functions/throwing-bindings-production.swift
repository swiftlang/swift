// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Production -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/production.h
// RUN: %FileCheck %s < %t/production.h
// RUN: %check-interop-cxx-header-in-clang(%t/production.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// Intentionally no swift_feature_ requirement: production compilers must
// accept the experimental feature flag too.
public func checkedValue() throws -> Int { 42 }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> checkedValue()
