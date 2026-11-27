// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/invalid.h
// RUN: %FileCheck %s < %t/invalid.h
// RUN: %check-interop-cxx-header-in-clang(%t/invalid.h)

// A module with errors still gets a header with
// -experimental-allow-module-with-compiler-errors. A function with an invalid
// parameter has no lowered signature to print.

public func duplicateNames(c: (), c: ()) {} // expected-error {{invalid redeclaration of 'c'}}
// expected-note@-1 {{'c' previously declared here}}

public func valid() {}

// CHECK-NOT: void duplicateNames(
// CHECK: SWIFT_INLINE_THUNK void valid() noexcept
// CHECK-NOT: void duplicateNames(
