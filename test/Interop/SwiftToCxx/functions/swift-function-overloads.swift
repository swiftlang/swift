// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-decls=all-public -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)



public func overloadedFunc(_ x: Int) { }
public func overloadedFunc(_ y: Float) { }

public func overloadedFuncArgLabel(x _: Int) { }
public func overloadedFuncArgLabel(y _: Float) { }

// CHECK-DAG: void overloadedFunc(float y) noexcept
// CHECK-DAG: void overloadedFunc(swift::Int x) noexcept
// CHECK-DAG: void overloadedFuncArgLabel(float _1) noexcept
// CHECK-DAG: void overloadedFuncArgLabel(swift::Int _1) noexcept
