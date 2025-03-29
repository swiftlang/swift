// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK:      SWIFT_INLINE_THUNK swift_double2 passThroughdouble2(swift_double2 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions18passThroughdouble2y4simd7double2VAEF(x);
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_float3 passThroughfloat3(swift_float3 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions17passThroughfloat3y4simd6float3VAEF(x);
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_float4 passThroughfloat4(swift_float4 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions17passThroughfloat4y4simd6float4VAEF(x);
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_int3 passThroughint3(swift_int3 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions15passThroughint3y4simd4int3VAEF(x);
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_uint4 passThroughuint4(swift_uint4 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Functions::_impl::$s9Functions16passThroughuint4y4simd5uint4VAEF(x);
// CHECK-NEXT: }

import simd

public func passThroughfloat3(_ x: float3) -> float3 { return x }
public func passThroughfloat4(_ x: float4) -> float4 { return x }
public func passThroughdouble2(_ x: double2) -> double2 { return x }
public func passThroughint3(_ x: int3) -> int3 { return x }
public func passThroughuint4(_ x: uint4) -> uint4 { return x }
