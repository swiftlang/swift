// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK:      inline float passThrougCFloat(float x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: return _impl::$s9Functions16passThrougCFloatyS2fF(x);
// CHECK-NEXT: }

// CHECK:      inline bool passThroughBool(bool x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions15passThroughBoolyS2bF(x);
// CHECK-NEXT: }

// CHECK:      inline bool passThroughCBool(bool x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughCBoolyS2bF(x);
// CHECK-NEXT: }

// CHECK:      inline char passThroughCChar(char x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughCCharys4Int8VADF(x);
// CHECK-NEXT: }

// CHECK:      inline char16_t passThroughCChar16(char16_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions18passThroughCChar16ys6UInt16VADF(x);
// CHECK-NEXT: }

// CHECK:      inline char32_t passThroughCChar32(char32_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions18passThroughCChar32ys7UnicodeO6ScalarVAFF(x);
// CHECK-NEXT: }

// CHECK:      inline double passThroughCDouble(double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions18passThroughCDoubleyS2dF(x);
// CHECK-NEXT: }

// CHECK:      inline int passThroughCInt(int x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions15passThroughCIntys5Int32VADF(x);
// CHECK-NEXT: }

// CHECK:      inline long long passThroughCLongLong(long long x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions20passThroughCLongLongys5Int64VADF(x);
// CHECK-NEXT: }

// CHECK:      inline short passThroughCShort(short x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions17passThroughCShortys5Int16VADF(x);
// CHECK-NEXT: }

// CHECK:      inline signed char passThroughCSignedChar(signed char x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions22passThroughCSignedCharys4Int8VADF(x);
// CHECK-NEXT: }


// CHECK:      inline unsigned int passThroughCUnsignedInt(unsigned int x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions23passThroughCUnsignedIntys6UInt32VADF(x);
// CHECK-NEXT: }

// CHECK:      inline unsigned long long passThroughCUnsignedLongLong(unsigned long long x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions024passThroughCUnsignedLongE0ys6UInt64VADF(x);
// CHECK-NEXT: }


// CHECK:      inline unsigned short passThroughCUnsignedShort(unsigned short x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions25passThroughCUnsignedShortys6UInt16VADF(x);
// CHECK-NEXT: }

// CHECK:      inline unsigned char passThroughCUnsignedSignedChar(unsigned char x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions30passThroughCUnsignedSignedCharys5UInt8VADF(x);
// CHECK-NEXT: }

// CHECK:      inline wchar_t passThroughCWideChar(wchar_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions20passThroughCWideCharys7UnicodeO6ScalarVAFF(x);
// CHECK-NEXT: }

// CHECK:      inline double passThroughDouble(double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions17passThroughDoubleyS2dF(x);
// CHECK-NEXT: }

// CHECK:      inline float passThroughFloat(float x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughFloatyS2fF(x);
// CHECK-NEXT: }

// CHECK:      inline float passThroughFloat32(float x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions18passThroughFloat32yS2fF(x);
// CHECK-NEXT: }

// CHECK:      inline double passThroughFloat64(double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions18passThroughFloat64yS2dF(x);
// CHECK-NEXT: }

// CHECK:      inline swift::Int passThroughInt(swift::Int x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions14passThroughIntyS2iF(x);
// CHECK-NEXT: }

// CHECK:      inline int16_t passThroughInt16(int16_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughInt16ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline int32_t passThroughInt32(int32_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughInt32ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline int64_t passThroughInt64(int64_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughInt64ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline int8_t passThroughInt8(int8_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions15passThroughInt8ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline void * _Nonnull passThroughOpaquePointer(void * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions24passThroughOpaquePointerys0dE0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline swift::UInt passThroughUInt(swift::UInt x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions15passThroughUIntyS2uF(x);
// CHECK-NEXT: }

// CHECK:      inline uint16_t passThroughUInt16(uint16_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions17passThroughUInt16ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline uint32_t passThroughUInt32(uint32_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions17passThroughUInt32ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline uint64_t passThroughUInt64(uint64_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions17passThroughUInt64ys0D0VADF(x);
// CHECK-NEXT: }


// CHECK:      inline uint8_t passThroughUInt8(uint8_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions16passThroughUInt8ys0D0VADF(x);
// CHECK-NEXT: }

// CHECK:      inline int32_t * _Nullable passThroughUnsafeGenericMutableOptionalPointer(int32_t * _Nullable x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions46passThroughUnsafeGenericMutableOptionalPointerySpys5Int32VGSgAFF(x);
// CHECK-NEXT: }

// CHECK:      inline int32_t * _Nonnull passThroughUnsafeGenericMutablePointer(int32_t * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions38passThroughUnsafeGenericMutablePointerySpys5Int32VGAEF(x);
// CHECK-NEXT: }

// CHECK:      inline int32_t const * _Nullable passThroughUnsafeGenericOptionalPointer(int32_t const * _Nullable x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions39passThroughUnsafeGenericOptionalPointerySPys5Int32VGSgAFF(x);
// CHECK-NEXT: }

// CHECK:      inline int32_t const * _Nonnull passThroughUnsafeGenericPointer(int32_t const * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions31passThroughUnsafeGenericPointerySPys5Int32VGAEF(x);
// CHECK-NEXT: }

// CHECK:      inline void * _Nonnull passThroughUnsafeMutableRawPointer(void * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions34passThroughUnsafeMutableRawPointeryS2vF(x);
// CHECK-NEXT: }

// CHECK:      inline void const * _Nonnull passThroughUnsafeRawPointer(void const * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions27passThroughUnsafeRawPointeryS2VF(x);
// CHECK-NEXT: }

// CHECK:      inline void * _Nullable roundTwoPassThroughUnsafeMutableRawPointer(void * _Nullable x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::$s9Functions42roundTwoPassThroughUnsafeMutableRawPointerySvSgACF(x);
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

public func passThrougCFloat(_ x: CFloat) -> CFloat { return x }
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

public func passThroughUnsafeGenericPointer(_ x: UnsafePointer<Int32>) -> UnsafePointer<Int32> {
    return x
}

public func passThroughUnsafeGenericOptionalPointer(_ x: UnsafePointer<Int32>?) -> UnsafePointer<Int32>? {
    return x
}

public func passThroughUnsafeGenericMutablePointer(_ x: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
    return x
}

public func passThroughUnsafeGenericMutableOptionalPointer(_ x: UnsafeMutablePointer<Int32>?) -> UnsafeMutablePointer<Int32>? {
    return x
}
