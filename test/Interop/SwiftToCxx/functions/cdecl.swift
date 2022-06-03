// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name CdeclFunctions -clang-header-expose-public-decls -emit-clang-header-path %t/cdecl.h
// RUN: %FileCheck %s < %t/cdecl.h

// RUN: %check-interop-cxx-header-in-clang(%t/cdecl.h)

// XFAIL: *

// CHECK-LABEL: namespace CdeclFunctions {

// CHECK: namespace _impl {
// CHECK: SWIFT_EXTERN int cfuncPassTwo(int x, int y) SWIFT_NOEXCEPT;
// CHECK: }

@_cdecl("cfuncPassTwo")
public func differentCDeclName(x: CInt, y: CInt) -> CInt { return x + y }

// CHECK: inline int differentCDeclName(int x, int y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK: return _impl::cfuncPassTwo(x, y);
// CHECK: }
