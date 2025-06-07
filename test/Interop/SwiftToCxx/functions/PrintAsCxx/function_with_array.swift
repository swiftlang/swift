// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/function_with_array.h
// RUN: %FileCheck %s < %t/function_with_array.h

// CHECK: namespace function_with_array
// FIXME: we don't actually emit a declaration for this, but for now at least
// check that we don't crash.
// CHECK: void f(const swift::Array<swift::Int>&
public func f(_: [Int]) { }
