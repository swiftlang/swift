// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions24transparentPrimitiveFuncyS2iF(ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL; // transparentPrimitiveFunc(_:)

// CHECK:      SWIFT_INLINE_THUNK swift::Int transparentPrimitiveFunc(swift::Int x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions24transparentPrimitiveFuncyS2iF(x);
// CHECK-NEXT: }

@_transparent
public func transparentPrimitiveFunc(_ x: Int) -> Int { return x * x }
