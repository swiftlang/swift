// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/invalid.h
// RUN: %FileCheck %s < %t/invalid.h
// RUN: %check-interop-cxx-header-in-clang(%t/invalid.h)

// A module with errors still gets a header with
// -experimental-allow-module-with-compiler-errors. A top-level subscript has no
// C++ name, so it isn't reported as unavailable either.

public subscript(i: Int) -> Int { 0 } // expected-error {{'subscript' functions may only be declared within a type}}

public func valid() {}

// CHECK-NOT: Unavailable in C++: Swift subscript
// CHECK: SWIFT_INLINE_THUNK void valid() noexcept
// CHECK-NOT: Unavailable in C++: Swift subscript
