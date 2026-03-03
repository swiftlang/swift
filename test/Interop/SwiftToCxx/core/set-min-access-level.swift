// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=all-public -emit-clang-header-min-access public -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-PUBLIC < %t/core.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=all-public -emit-clang-header-min-access package -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-PACKAGE < %t/core.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=all-public -emit-clang-header-min-access internal -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-INTERNAL < %t/core.h

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend %s -module-name Core -typecheck -emit-clang-header-path %t/core.h -clang-header-expose-decls=all-public -emit-clang-header-min-access inernal -package-name Core 2>&1 | %FileCheck %s --check-prefix CHECK-DIAGNOSTIC

public func publicFunc(_ x: Int) -> Int {
    return x
}

package func packageFunc(_ x: Int) -> Int {
    return x
}

internal func internalFunc(_ x: Int) -> Int {
    return x
}

private func privateFunc(_ x: Int) -> Int {
    return x
}

public struct S {
    public func publicMethod(_ x: Int) -> Int {
        return x
    }

    package func packageMethod(_ x: Int) -> Int {
        return x
    }

    internal func internalMethod(_ x: Int) -> Int {
        return x
    }

    private func privateMethod(_ x: Int) -> Int {
        return x
    }

    private var x: Int
}

// CHECK-PUBLIC-NOT: internalFunc
// CHECK-PUBLIC-NOT: packageFunc
// CHECK-PUBLIC-NOT: privateFunc
// CHECK-PUBLIC: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-PUBLIC: SWIFT_INLINE_THUNK swift::Int S::publicMethod(swift::Int x) const {
// CHECK-PUBLIC-NOT: packageMethod
// CHECK-PUBLIC-NOT: internalMethod
// CHECK-PUBLIC-NOT: privateMethod

// CHECK-PACKAGE-NOT: internalFunc
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int packageFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core11packageFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-PACKAGE-NOT: privateFunc
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int S::publicMethod(swift::Int x) const {
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int S::packageMethod(swift::Int x) const {
// CHECK-PACKAGE-NOT: internalMethod
// CHECK-PACKAGE-NOT: privateMethod

// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int internalFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core12internalFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int packageFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core11packageFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-INTERNAL-NOT: privateFunc
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int S::publicMethod(swift::Int x) const {
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int S::packageMethod(swift::Int x) const {
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int S::internalMethod(swift::Int x) const {
// CHECK-INTERNAL-NOT: privateMethod

// CHECK-DIAGNOSTIC: error: invalid minimum clang header access level 'inernal'; chose from 'public'|'package'|'internal'
