// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature CxxExistentialInterop
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_UPCOMING_SWIFT -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_CxxExistentialInterop

// expected-no-diagnostics

public protocol Drawable {
    func draw() -> Int
}

public struct Circle: Drawable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
}

// Existential parameter.
public func drawTwice(_ d: any Drawable) -> Int {
    return d.draw() + d.draw()
}

// Existential return.
public func bestDrawable(_ a: any Drawable, _ b: any Drawable) -> any Drawable {
    return a.draw() >= b.draw() ? a : b
}

// --- C prologue: bestDrawable (alphabetical order) ---

// CHECK-LABEL: SWIFT_EXTERN void $s9Functions12bestDrawable{{[^(]+}}(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull a, const void * _Nonnull b) SWIFT_NOEXCEPT SWIFT_CALL;

// --- C prologue: drawTwice ---

// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions9drawTwice{{[^(]+}}(const void * _Nonnull d) SWIFT_NOEXCEPT SWIFT_CALL;

// --- C++ thunks (guarded, alphabetical order) ---

// CHECK: #if defined(SWIFT_CXX_EXISTENTIAL_INTEROP) && defined(__cpp_concepts) && __cpp_concepts >= 202002L
// CHECK: Drawable bestDrawable(const Drawable& a, const Drawable& b)
// CHECK:   return _impl::_impl_Drawable::returnNewValue
// CHECK: #endif

// CHECK: #if defined(SWIFT_CXX_EXISTENTIAL_INTEROP) && defined(__cpp_concepts) && __cpp_concepts >= 202002L
// CHECK: swift::Int drawTwice(const Drawable& d)
// CHECK:   _impl::_impl_Drawable::getOpaquePointer(d)
// CHECK: #endif
