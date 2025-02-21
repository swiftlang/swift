// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name CdeclFunctions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/cdecl.h
// RUN: %FileCheck %s < %t/cdecl.h

// RUN: %check-interop-cxx-header-in-clang(%t/cdecl.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// CHECK-LABEL: namespace CdeclFunctions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("CdeclFunctions") {

// CHECK: namespace _impl {
// CHECK: SWIFT_EXTERN int cfuncPassTwo(int x, int y) SWIFT_NOEXCEPT;
// CHECK: }

@_cdecl("cfuncPassTwo")
public func differentCDeclName(x: CInt, y: CInt) -> CInt { return x + y }

// CHECK: SWIFT_INLINE_THUNK int differentCDeclName(int x, int y) noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_WARN_UNUSED_RESULT {
// CHECK: return CdeclFunctions::_impl::cfuncPassTwo(x, y);
// CHECK: }
