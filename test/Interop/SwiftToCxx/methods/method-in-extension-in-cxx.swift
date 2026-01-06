// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Method -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/methods.h
// RUN: %FileCheck %s < %t/methods.h

// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct OverloadedMethodInExtension {
    var x: Int

    public mutating func add(_ x: Int) {
        self.x += x
    }
}
extension OverloadedMethodInExtension {
    public mutating func add(viaExtension x: Int) {
        self.x += x
    }
}

// Make sure we don't emit ambiguous overloads:
// CHECK: SWIFT_INLINE_THUNK void OverloadedMethodInExtension::add(swift::Int x)
// CHECK-NOT: SWIFT_INLINE_THUNK void OverloadedMethodInExtension::add(swift::Int x)
