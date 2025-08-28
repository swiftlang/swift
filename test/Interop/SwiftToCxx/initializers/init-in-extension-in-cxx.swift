// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/inits.h
// RUN: %FileCheck %s < %t/inits.h

// RUN: %check-interop-cxx-header-in-clang(%t/inits.h -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct OverloadedInitInExtension {
    let x: Int

    public init(_ x: Int) {
        self.x = x
    }
}
extension OverloadedInitInExtension {
    public init(viaExtension x: Int) {
        self.x = x
    }
}

// Make sure we don't emit ambiguous overloads:
// CHECK: static SWIFT_INLINE_THUNK OverloadedInitInExtension init(swift::Int x)
// CHECK-NOT: static SWIFT_INLINE_THUNK OverloadedInitInExtension init(swift::Int x)
