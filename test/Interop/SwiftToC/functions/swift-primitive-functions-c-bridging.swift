// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s -check-prefix CHECK -check-prefix CHECK-%target-abi < %t/functions.h

// RUN: %check-interop-c-header-in-clang(%t/functions.h)

// CHECK: SWIFT_EXTERN bool $s9Functions15passThroughBoolyS2bF(bool x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN bool $s9Functions16passThroughCBoolyS2bF(bool x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN char $s9Functions16passThroughCCharys4Int8VADF(char x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN char16_t $s9Functions18passThroughCChar16ys6UInt16VADF(char16_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN char32_t $s9Functions18passThroughCChar32ys7UnicodeO6ScalarVAFF(char32_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN double $s9Functions18passThroughCDoubleyS2dF(double x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN float $s9Functions17passThroughCFloatyS2fF(float x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN int $s9Functions15passThroughCIntys5Int32VADF(int x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN long long $s9Functions20passThroughCLongLongys5Int64VADF(long long x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN short $s9Functions17passThroughCShortys5Int16VADF(short x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN signed char $s9Functions22passThroughCSignedCharys4Int8VADF(signed char x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN unsigned int $s9Functions23passThroughCUnsignedIntys6UInt32VADF(unsigned int x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN unsigned long long $s9Functions024passThroughCUnsignedLongE0ys6UInt64VADF(unsigned long long x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN unsigned short $s9Functions25passThroughCUnsignedShortys6UInt16VADF(unsigned short x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN unsigned char $s9Functions30passThroughCUnsignedSignedCharys5UInt8VADF(unsigned char x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-SYSV-NEXT: SWIFT_EXTERN wchar_t $s9Functions20passThroughCWideCharys7UnicodeO6ScalarVAFF(wchar_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-WIN-NEXT: SWIFT_EXTERN wchar_t $s9Functions20passThroughCWideCharys6UInt16VADF(wchar_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN double $s9Functions17passThroughDoubleyS2dF(double x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN float $s9Functions16passThroughFloatyS2fF(float x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN float $s9Functions18passThroughFloat32yS2fF(float x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN double $s9Functions18passThroughFloat64yS2dF(double x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN ptrdiff_t $s9Functions14passThroughIntyS2iF(ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN int16_t $s9Functions16passThroughInt16ys0D0VADF(int16_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN int32_t $s9Functions16passThroughInt32ys0D0VADF(int32_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN int64_t $s9Functions16passThroughInt64ys0D0VADF(int64_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN int8_t $s9Functions15passThroughInt8ys0D0VADF(int8_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN void * _Nonnull $s9Functions24passThroughOpaquePointerys0dE0VADF(void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN size_t $s9Functions15passThroughUIntyS2uF(size_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN uint16_t $s9Functions17passThroughUInt16ys0D0VADF(uint16_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN uint32_t $s9Functions17passThroughUInt32ys0D0VADF(uint32_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN uint64_t $s9Functions17passThroughUInt64ys0D0VADF(uint64_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN uint8_t $s9Functions16passThroughUInt8ys0D0VADF(uint8_t x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN void * _Nonnull $s9Functions34passThroughUnsafeMutableRawPointeryS2vF(void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN void const * _Nonnull $s9Functions27passThroughUnsafeRawPointeryS2VF(void const * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN void * _Nullable $s9Functions42roundTwoPassThroughUnsafeMutableRawPointerySvSgACF(void * _Nullable x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-EMPTY:
// CHECK-NEXT: #ifdef __cplusplus
// CHECK-NEXT: }
// CHECK-NEXT: }

public func passThroughCBool(_ x: CBool) -> CBool { return x }

public func passThroughCChar(_ x: CChar) -> CChar { return x }
public func passThroughCWideChar(_ x: CWideChar) -> CWideChar { return x }
public func passThroughCChar16(_ x: CChar16) -> CChar16 { return x }
public func passThroughCChar32(_ x: CChar32) -> CChar32 { return x }

// Don't test CLong as it's platform specific. See long-lp64 test instead.
public func passThroughCSignedChar(_ x: CSignedChar) -> CSignedChar { return x }
public func passThroughCShort(_ x: CShort) -> CShort { return x }
public func passThroughCInt(_ x: CInt) -> CInt { return x }
public func passThroughCLongLong(_ x: CLongLong) -> CLongLong { return x }

// Don't test CUnsignedLong as it's platform specific. See long-lp64 test instead.
public func passThroughCUnsignedSignedChar(_ x: CUnsignedChar) -> CUnsignedChar { return x }
public func passThroughCUnsignedShort(_ x: CUnsignedShort) -> CUnsignedShort { return x }
public func passThroughCUnsignedInt(_ x: CUnsignedInt) -> CUnsignedInt { return x }
public func passThroughCUnsignedLongLong(_ x: CUnsignedLongLong) -> CUnsignedLongLong { return x }

public func passThroughCFloat(_ x: CFloat) -> CFloat { return x }
public func passThroughCDouble(_ x: CDouble) -> CDouble { return x }

public func passThroughInt8(_ x: Int8) -> Int8 { return x }
public func passThroughInt16(_ x: Int16) -> Int16 { return x }
public func passThroughInt32(_ x: Int32) -> Int32 { return x }
public func passThroughInt64(_ x: Int64) -> Int64 { return x }

public func passThroughUInt8(_ x: UInt8) -> UInt8 { return x }
public func passThroughUInt16(_ x: UInt16) -> UInt16 { return x }
public func passThroughUInt32(_ x: UInt32) -> UInt32 { return x }
public func passThroughUInt64(_ x: UInt64) -> UInt64 { return x }

public func passThroughFloat(_ x: Float) -> Float { return x }
public func passThroughDouble(_ x: Double) -> Double { return x }
public func passThroughFloat32(_ x: Float32) -> Float32 { return x }
public func passThroughFloat64(_ x: Float64) -> Float64 { return x }

public func passThroughInt(_ x: Int) -> Int { return x }
public func passThroughUInt(_ x: UInt) -> UInt { return x }
public func passThroughBool(_ x: Bool) -> Bool { return x }

public func passThroughOpaquePointer(_ x: OpaquePointer) -> OpaquePointer { return x }
public func passThroughUnsafeRawPointer(_ x: UnsafeRawPointer) -> UnsafeRawPointer { return x }
public func passThroughUnsafeMutableRawPointer(_ x: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer { return x }

public func roundTwoPassThroughUnsafeMutableRawPointer(_ x: UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer? { return x }
