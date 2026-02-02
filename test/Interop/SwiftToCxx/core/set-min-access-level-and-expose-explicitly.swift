// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access public -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-PUBLIC < %t/core.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access package -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-PACKAGE < %t/core.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -typecheck -verify -emit-clang-header-path %t/core.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal -package-name Core
// RUN: %FileCheck %s --check-prefix CHECK-INTERNAL < %t/core.h

@_expose(Cxx)
public func publicFunc(_ x: Int) -> Int {
    return x
}

@_expose(Cxx)
package func packageFunc(_ x: Int) -> Int {
    return x
}

@_expose(Cxx)
internal func internalFunc(_ x: Int) -> Int {
    return x
}

@_expose(Cxx)
private func privateFunc(_ x: Int) -> Int {
    return x
}

// CHECK-PUBLIC-NOT: internalFunc
// CHECK-PUBLIC-NOT: packageFunc
// CHECK-PUBLIC-NOT: privateFunc
// CHECK-PUBLIC: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {

// CHECK-PACKAGE-NOT: internalFunc
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int packageFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core11packageFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-PACKAGE-NOT: privateFunc
// CHECK-PACKAGE: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {

// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int internalFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core12internalFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int packageFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core11packageFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-INTERNAL-NOT: privateFunc
// CHECK-INTERNAL: SWIFT_INLINE_THUNK swift::Int publicFunc(swift::Int x) noexcept SWIFT_SYMBOL("s:4Core10publicFuncyS2iF") SWIFT_WARN_UNUSED_RESULT {
