// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// CHECK: SWIFT_EXTERN void $s9Functions19testKeywordArgument8registerySi_tF(ptrdiff_t register_) SWIFT_NOEXCEPT SWIFT_CALL;

public func testKeywordArgument(register: Int) { }
