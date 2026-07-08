// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature CxxExistentialInterop
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_UPCOMING_SWIFT -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_CxxExistentialInterop

// expected-no-diagnostics

public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize() -> Int
}

public struct Widget: Drawable, Resizable {
    var size: Int
    public init(size: Int) { self.size = size }
    public func draw() -> Int { return size }
    public func resize() -> Int { return size * 2 }
}

// Composition parameter.
public func drawAndResize(_ x: any Drawable & Resizable) -> Int {
    return x.draw() + x.resize()
}

// Composition return.
public func makeDrawableAndResizable() -> any Drawable & Resizable {
    return Widget(size: 42)
}

// --- Composition wrapper class ---

// CHECK-LABEL: class AnyDrawableAndResizable final : public swift::_impl::SwiftExistentialType<_impl::DrawableTag, _impl::ResizableTag>
// CHECK: draw() const
// CHECK: resize() const
// CHECK: Drawable asDrawable() const
// CHECK: Resizable asResizable() const

// --- _impl helper class ---

// CHECK-LABEL: class _impl_AnyDrawableAndResizable
// CHECK: static {{.*}} AnyDrawableAndResizable _fromExistential(
// CHECK: static {{.*}} const char * _Nonnull getOpaquePointer(const AnyDrawableAndResizable &object)
// CHECK: static {{.*}} AnyDrawableAndResizable returnNewValue

// --- Composition parameter: drawAndResize ---

// CHECK-LABEL: SWIFT_INLINE_THUNK swift::Int drawAndResize
// CHECK-SAME: const Functions::AnyDrawableAndResizable&
// CHECK: _impl::_impl_AnyDrawableAndResizable::getOpaquePointer(

// --- Composition return: makeDrawableAndResizable ---

// CHECK-LABEL: SWIFT_INLINE_THUNK {{.*}}AnyDrawableAndResizable makeDrawableAndResizable
// CHECK: _impl::_impl_AnyDrawableAndResizable::returnNewValue
