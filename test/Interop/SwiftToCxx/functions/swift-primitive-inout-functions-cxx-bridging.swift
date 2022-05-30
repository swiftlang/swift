// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK:      inline void inOutBool(bool & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions9inOutBoolyySbzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCBool(bool & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutCBoolyySbzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCChar(char & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutCCharyys4Int8VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCChar16(char16_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions12inOutCChar16yys6UInt16VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCChar32(char32_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions12inOutCChar32yys7UnicodeO6ScalarVzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCDouble(double & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions12inOutCDoubleyySdzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCFloat(float & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutCFloatyySfzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCInt(int & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions9inOutCIntyys5Int32VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCLongLong(long long & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions14inOutCLongLongyys5Int64VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCShort(short & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutCShortyys5Int16VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCSignedChar(signed char & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions16inOutCSignedCharyys4Int8VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCUnsignedChar(unsigned char & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions18inOutCUnsignedCharyys5UInt8VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCUnsignedInt(unsigned int & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions17inOutCUnsignedIntyys6UInt32VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCUnsignedLongLong(unsigned long long & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions018inOutCUnsignedLongE0yys6UInt64VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCUnsignedShort(unsigned short & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions19inOutCUnsignedShortyys6UInt16VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutCWideChar(wchar_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions14inOutCWideCharyys7UnicodeO6ScalarVzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutDouble(double & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutDoubleyySdzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutFloat(float & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutFloatyySfzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutFloat32(float & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions12inOutFloat32yySfzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutFloat64(double & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions12inOutFloat64yySdzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutInt(swift::Int & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions8inOutIntyySizF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutInt16(int16_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutInt16yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutInt32(int32_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutInt32yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutInt64(int64_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutInt64yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutInt8(int8_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions9inOutInt8yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutOpaquePointer(void * _Nonnull & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions18inOutOpaquePointeryys0dE0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutTwoInt(swift::Int & x, swift::Int & y) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutTwoIntyySiz_SiztF(x, y);
// CHECK-NEXT: }

// CHECK:      inline void inOutTwoParam(bool & x, double & y) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions13inOutTwoParamyySbz_SdztF(x, y);
// CHECK-NEXT: }

// CHECK:      inline void inOutUInt(swift::UInt & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions9inOutUIntyySuzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUInt16(uint16_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutUInt16yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUInt32(uint32_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutUInt32yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUInt64(uint64_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutUInt64yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUInt8(uint8_t & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions10inOutUInt8yys0D0VzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUnsafeMutableRawPointer(void * _Nonnull & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions28inOutUnsafeMutableRawPointeryySvzF(x);
// CHECK-NEXT: }

// CHECK:      inline void inOutUnsafeRawPointer(void const * _Nonnull & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions21inOutUnsafeRawPointeryySVzF(x);
// CHECK-NEXT: }

// CHECK:      inline void roundTwoInOutUnsafeMutableRawPointer(void * _Nullable & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions36roundTwoInOutUnsafeMutableRawPointeryySvSgzF(x);
// CHECK-NEXT: }

public func inOutCBool(_ x: inout CBool) { x = CBool() }

public func inOutCChar(_ x: inout CChar) { x = CChar() }
public func inOutCWideChar(_ x: inout CWideChar) { x = Unicode.Scalar(0) }  // init() is not available
public func inOutCChar16(_ x: inout CChar16) { x = CChar16() }
public func inOutCChar32(_ x: inout CChar32) { x = Unicode.Scalar(0) }  // init() is not available

// Don't test CLong as it's platform specific. See long-lp64 test instead.
public func inOutCSignedChar(_ x: inout CSignedChar) { x = CSignedChar() }
public func inOutCShort(_ x: inout CShort) { x = CShort() }
public func inOutCInt(_ x: inout CInt) { x = CInt() }
public func inOutCLongLong(_ x: inout CLongLong) { x = CLongLong() }

// Don't test CUnsignedLong as it's platform specific. See long-lp64 test instead.
public func inOutCUnsignedChar(_ x: inout CUnsignedChar) { x = CUnsignedChar() }
public func inOutCUnsignedShort(_ x: inout CUnsignedShort) { x = CUnsignedShort() }
public func inOutCUnsignedInt(_ x: inout CUnsignedInt) { x = CUnsignedInt() }
public func inOutCUnsignedLongLong(_ x: inout CUnsignedLongLong) { x = CUnsignedLongLong() }

public func inOutCFloat(_ x: inout CFloat) { x = CFloat() }
public func inOutCDouble(_ x: inout CDouble) { x = CDouble() }

public func inOutInt8(_ x: inout Int8) { x = Int8() }
public func inOutInt16(_ x: inout Int16) { x = Int16() }
public func inOutInt32(_ x: inout Int32) { x = Int32() }
public func inOutInt64(_ x: inout Int64) { x = Int64() }

public func inOutUInt8(_ x: inout UInt8) { x = UInt8() }
public func inOutUInt16(_ x: inout UInt16) { x = UInt16() }
public func inOutUInt32(_ x: inout UInt32) { x = UInt32() }
public func inOutUInt64(_ x: inout UInt64) { x = UInt64() }

public func inOutFloat(_ x: inout Float) { x = Float() }
public func inOutDouble(_ x: inout Double) { x = Double() }
public func inOutFloat32(_ x: inout Float32) { x = Float32() }
public func inOutFloat64(_ x: inout Float64) { x = Float64() }

public func inOutInt(_ x: inout Int) { x = Int() }
public func inOutUInt(_ x: inout UInt) { x = UInt() }
public func inOutBool(_ x: inout Bool) { x = Bool() }

public func inOutOpaquePointer(_ x: inout OpaquePointer) { x = OpaquePointer(UnsafeRawPointer(x)) }
public func inOutUnsafeRawPointer(_ x: inout UnsafeRawPointer) { x = UnsafeRawPointer(x) }
public func inOutUnsafeMutableRawPointer(_ x: inout UnsafeMutableRawPointer) { x = UnsafeMutableRawPointer(x) }

public func roundTwoInOutUnsafeMutableRawPointer(_ x: inout UnsafeMutableRawPointer?) { x = nil }

public func inOutTwoInt(_ x: inout Int, _ y: inout Int) {
    x += y
    y -= 2 * x
}

public func inOutTwoParam(_ x: inout Bool, _ y: inout Double) {
    y = 3.14
    x = !x
}
