// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK: inline void testKeywordArgument(swift::Int register_) noexcept
// CHECK-NEXT: return _impl::$s9Functions19testKeywordArgument8registerySi_tF(register_);

func testKeywordArgument(register: Int) {
}
