// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/use-cxx-types.swift -module-name UseCxxTy -typecheck -verify -emit-clang-header-path %t/UseCxxTy.h -I %t -enable-experimental-cxx-interop -disable-availability-checking -Xcc -DSIMD_NO_CODE

// RUN: %FileCheck %s < %t/UseCxxTy.h

// FIXME: remove once https://github.com/apple/swift/pull/60971 lands.
// RUN: echo "#include \"header.h\"" > %t/full-cxx-swift-cxx-bridging.h
// RUN: cat %t/UseCxxTy.h >> %t/full-cxx-swift-cxx-bridging.h

// RUN: %check-interop-cxx-header-in-clang(%t/full-cxx-swift-cxx-bridging.h -Wno-reserved-identifier -DSIMD_NO_CODE)

// This is required to verify that `Struct` is returned and passed directly.
// REQUIRES: OS=macosx
// REQUIRES: PTRSIZE=64

//--- header.h

#include <simd.h>

using simd_float4x4 = float4x4;

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public struct Struct {
    private let transform: simd_float4x4

    public init() {
        transform = simd_float4x4()
    }
}

public func passStruct(_ x : Struct) {
    
}

// CHECK: class SWIFT_SYMBOL("s:8UseCxxTy6StructV") Struct final {

// CHECK: SWIFT_INLINE_THUNK void passStruct(const Struct& x) noexcept SWIFT_SYMBOL("s:8UseCxxTy10passStructyyAA0E0VF") {
// CHECK-NEXT:   UseCxxTy::_impl::$s8UseCxxTy10passStructyyAA0E0VF(UseCxxTy::_impl::swift_interop_passDirect_UseCxxTy_swift_float4_0_16_swift_float4_16_32_swift_float4_32_48_swift_float4_48_64(UseCxxTy::_impl::_impl_Struct::getOpaquePointer(x)));
// CHECK-NEXT:}

// CHECK:  SWIFT_INLINE_THUNK Struct Struct::init() {
// CHECK-NEXT:  return UseCxxTy::_impl::_impl_Struct::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT:    UseCxxTy::_impl::swift_interop_returnDirect_UseCxxTy_swift_float4_0_16_swift_float4_16_32_swift_float4_32_48_swift_float4_48_64(result, UseCxxTy::_impl::$s8UseCxxTy6StructVACycfC());
// CHECK-NEXT:  });
// CHECK-NEXT:  }

