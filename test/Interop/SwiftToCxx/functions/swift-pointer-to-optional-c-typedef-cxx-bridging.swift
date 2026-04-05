// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -I %S/Inputs/ -typecheck -verify -emit-clang-header-path %t/functions.h -disable-availability-checking

// RUN: %FileCheck %s < %t/functions.h

import CPointerTypes

public func takesPointerToStruct(_ x: UnsafeMutablePointer<CGImageBlock>) {
}

public func takesPointerToOptionalRef(_ x: UnsafeMutablePointer<CGImageBlockRef?>) {
}

public func takesPointerToOptionalForeignRef(_ x: UnsafeMutablePointer<FRT?>) {
}

public func takesOptionalRef(_ x: CGImageBlockRef?) {
}

public func takesOptionalForeignRef(_ x: FRT?) {
}

// CHECK: SWIFT_INLINE_THUNK void takesOptionalForeignRef(FRT *_Nullable x) noexcept
// CHECK: SWIFT_INLINE_THUNK void takesOptionalRef(CGImageBlock * _Nullable x) noexcept
// CHECK: SWIFT_INLINE_THUNK void takesPointerToOptionalForeignRef(FRT *_Nullable * _Nonnull x) noexcept
// CHECK: SWIFT_INLINE_THUNK void takesPointerToOptionalRef(CGImageBlock * _Nullable * _Nonnull x) noexcept
// CHECK: SWIFT_INLINE_THUNK void takesPointerToStruct(CGImageBlock * _Nonnull x) noexcept
