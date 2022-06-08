// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// CHECK: SWIFT_EXTERN void $s9Functions19testKeywordArgument8registerySi_tF(ptrdiff_t register_) SWIFT_NOEXCEPT SWIFT_CALL;

func testKeywordArgument(register: Int) { }
