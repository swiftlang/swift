// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseSIMD -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/UseSIMD.h
// RUN: %FileCheck %s < %t/UseSIMD.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/UseSIMD.h -o %t/o.o

// REQUIRES: objc_interop
// REQUIRES: rdar157848231

import simd

public func swiftThingScalar(a: Float) {}

public func swiftThingSIMD(a: float3) {
    print(a)
}

public func swiftThingSIMD2(a: simd_float3) {
    print(a)
}

public func swiftThingSIMD3() -> float3 {
    float3(4, 5, 6)
}

public func swiftThingSIMD4() -> simd_float3 {
    simd_float3(4, 5, 6)
}

// CHECK: SWIFT_EXTERN void $s7UseSIMD010swiftThingB01ays5SIMD3VySfG_tF(struct swift_interop_passStub_UseSIMD_swift_float4_0_16 a) SWIFT_NOEXCEPT SWIFT_CALL; // swiftThingSIMD(a:)
// CHECK: SWIFT_EXTERN void $s7UseSIMD15swiftThingSIMD21ays5SIMD3VySfG_tF(struct swift_interop_passStub_UseSIMD_swift_float4_0_16 a) SWIFT_NOEXCEPT SWIFT_CALL; // swiftThingSIMD2(a:)
// CHECK: SWIFT_EXTERN struct swift_interop_returnStub_UseSIMD_swift_float4_0_16 $s7UseSIMD15swiftThingSIMD3s0E0VySfGyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // swiftThingSIMD3()
// CHECK: SWIFT_EXTERN struct swift_interop_returnStub_UseSIMD_swift_float4_0_16 $s7UseSIMD15swiftThingSIMD4s5SIMD3VySfGyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // swiftThingSIMD4()
// CHECK: SWIFT_EXTERN void $s7UseSIMD16swiftThingScalar1aySf_tF(float a) SWIFT_NOEXCEPT SWIFT_CALL; // swiftThingScalar(a:)

// CHECK:      SWIFT_INLINE_THUNK void swiftThingSIMD(swift_float3 a) noexcept SWIFT_SYMBOL("s:7UseSIMD010swiftThingB01ays5SIMD3VySfG_tF") {
// CHECK-NEXT:   UseSIMD::_impl::$s7UseSIMD010swiftThingB01ays5SIMD3VySfG_tF(UseSIMD::_impl::swift_interop_passDirect_UseSIMD_swift_float4_0_16(reinterpret_cast<const char *>(swift::_impl::getOpaquePointer(a))));
// CHECK-NEXT: }

// CHECK:      WIFT_INLINE_THUNK void swiftThingSIMD2(swift_float3 a) noexcept SWIFT_SYMBOL("s:7UseSIMD15swiftThingSIMD21ays5SIMD3VySfG_tF") {
// CHECK-NEXT:   UseSIMD::_impl::$s7UseSIMD15swiftThingSIMD21ays5SIMD3VySfG_tF(UseSIMD::_impl::swift_interop_passDirect_UseSIMD_swift_float4_0_16(reinterpret_cast<const char *>(swift::_impl::getOpaquePointer(a))));
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_float3 swiftThingSIMD3() noexcept SWIFT_SYMBOL("s:7UseSIMD15swiftThingSIMD3s0E0VySfGyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(swift_float3)) char storage[sizeof(swift_float3)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<swift_float3 *>(storage);
// CHECK-NEXT: UseSIMD::_impl::swift_interop_returnDirect_UseSIMD_swift_float4_0_16(storage, UseSIMD::_impl::$s7UseSIMD15swiftThingSIMD3s0E0VySfGyF());
// CHECK-NEXT: return *storageObjectPtr;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK swift_float3 swiftThingSIMD4() noexcept SWIFT_SYMBOL("s:7UseSIMD15swiftThingSIMD4s5SIMD3VySfGyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(swift_float3)) char storage[sizeof(swift_float3)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<swift_float3 *>(storage);
// CHECK-NEXT: UseSIMD::_impl::swift_interop_returnDirect_UseSIMD_swift_float4_0_16(storage, UseSIMD::_impl::$s7UseSIMD15swiftThingSIMD4s5SIMD3VySfGyF());
// CHECK-NEXT: return *storageObjectPtr;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK void swiftThingScalar(float a) noexcept SWIFT_SYMBOL("s:7UseSIMD16swiftThingScalar1aySf_tF") {
// CHECK-NEXT:   UseSIMD::_impl::$s7UseSIMD16swiftThingScalar1aySf_tF(a);
// CHECK-NEXT: }
