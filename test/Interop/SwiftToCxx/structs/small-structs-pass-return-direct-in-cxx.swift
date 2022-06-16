// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h)

public struct StructOneI64 {
    let x: Int64
}

public struct StructTwoI32 {
    let x, y: Int32
}

public struct StructOneI16AndOneStruct {
    var x: Int16
    var y: StructTwoI32
}

public struct StructU16AndPointer {
    let x: UInt8
    let y: UnsafeMutableRawPointer
}

public struct StructDoubleAndFloat {
    var x: Double
    var y: Float
}

// CHECK: SWIFT_EXTERN void $s7Structs25inoutStructDoubleAndFloatyyAA0cdeF0VzF(char * _Nonnull s) SWIFT_NOEXCEPT SWIFT_CALL; // inoutStructDoubleAndFloat(_:)
// CHECK: SWIFT_EXTERN void $s7Structs020inoutStructOneI16AnddC0yyAA0cdefdC0Vz_AA0C6TwoI32VtF(char * _Nonnull s, struct swift_interop_stub_Structs_StructTwoI32 s2) SWIFT_NOEXCEPT SWIFT_CALL; // inoutStructOneI16AndOneStruct(_:_:)

// CHECK: class StructDoubleAndFloat final {

// CHECK: class StructOneI16AndOneStruct final {

// CHECK: class StructOneI64 final {

// CHECK: class StructTwoI32 final {

// CHECK: class StructU16AndPointer final {

public func returnNewStructOneI64() -> StructOneI64 { return StructOneI64(x: 42 ) }

public func passThroughStructOneI64(_ x: StructOneI64) -> StructOneI64 { return x }

public func printStructOneI64(_ x: StructOneI64) {
    print("StructOneI64.x = \(x.x)")
}

public func returnNewStructTwoI32(_ x: Int32) -> StructTwoI32 { return StructTwoI32(x: x, y: x * 2) }

public func passThroughStructTwoI32(_ i: Int32, _ x: StructTwoI32, _ j: Int32) -> StructTwoI32 {
    return StructTwoI32(x: x.x + i, y: x.y + j)
}

public func printStructTwoI32(_ x: StructTwoI32) {
    print("StructTwoI32.x = \(x.x), y = \(x.y)")
}

public func returnNewStructOneI16AndOneStruct() -> StructOneI16AndOneStruct {
    return StructOneI16AndOneStruct(x: 0xFF, y: StructTwoI32(x: 5, y: 72))
}

public func printStructStructTwoI32_and_OneI16AndOneStruct(_ y: StructTwoI32, _ x: StructOneI16AndOneStruct) {
    printStructTwoI32(y)
    print("StructOneI16AndOneStruct.x = \(x.x), y.x = \(x.y.x), y.y = \(x.y.y)")
}

public func inoutStructOneI16AndOneStruct(_ s: inout StructOneI16AndOneStruct, _ s2: StructTwoI32) {
    s.x -= 50
    s.y = s2
}

public func returnNewStructU16AndPointer(_ x: UnsafeMutableRawPointer) -> StructU16AndPointer {
    return StructU16AndPointer(x: 55, y: x)
}

public func getStructU16AndPointer_x(_ x: StructU16AndPointer) -> UInt8 { return x.x }

public func getStructU16AndPointer_y(_ x: StructU16AndPointer) -> UnsafeMutableRawPointer { return x.y }

public func returnNewStructDoubleAndFloat(_ y: Float, _ x: Double) -> StructDoubleAndFloat {
    return StructDoubleAndFloat(x: x, y: y)
}

public func getStructDoubleAndFloat_x(_ x: StructDoubleAndFloat) -> Double { return x.x }

public func getStructDoubleAndFloat_y(_ x: StructDoubleAndFloat) -> Float { return x.y }

public func inoutStructDoubleAndFloat(_ s: inout StructDoubleAndFloat) {
    s.x *= Double(s.y)
    s.y /= 10
}

// CHECK: inline double getStructDoubleAndFloat_x(const StructDoubleAndFloat& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::$s7Structs25getStructDoubleAndFloat_xySdAA0cdeF0VF(_impl::swift_interop_passDirect_Structs_StructDoubleAndFloat(_impl::_impl_StructDoubleAndFloat::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline float getStructDoubleAndFloat_y(const StructDoubleAndFloat& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::$s7Structs25getStructDoubleAndFloat_yySfAA0cdeF0VF(_impl::swift_interop_passDirect_Structs_StructDoubleAndFloat(_impl::_impl_StructDoubleAndFloat::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline uint8_t getStructU16AndPointer_x(const StructU16AndPointer& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::$s7Structs24getStructU16AndPointer_xys5UInt8VAA0cdeF0VF(_impl::swift_interop_passDirect_Structs_StructU16AndPointer(_impl::_impl_StructU16AndPointer::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline void * _Nonnull getStructU16AndPointer_y(const StructU16AndPointer& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::$s7Structs24getStructU16AndPointer_yySvAA0cdeF0VF(_impl::swift_interop_passDirect_Structs_StructU16AndPointer(_impl::_impl_StructU16AndPointer::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK:      inline void inoutStructDoubleAndFloat(StructDoubleAndFloat& s) noexcept {
// CHECK-NEXT:   return _impl::$s7Structs25inoutStructDoubleAndFloatyyAA0cdeF0VzF(_impl::_impl_StructDoubleAndFloat::getOpaquePointer(s));
// CHECK-NEXT: }


// CHECK:      inline void inoutStructOneI16AndOneStruct(StructOneI16AndOneStruct& s, const StructTwoI32& s2) noexcept {
// CHECK-NEXT:   return _impl::$s7Structs020inoutStructOneI16AnddC0yyAA0cdefdC0Vz_AA0C6TwoI32VtF(_impl::_impl_StructOneI16AndOneStruct::getOpaquePointer(s), _impl::swift_interop_passDirect_Structs_StructTwoI32(_impl::_impl_StructTwoI32::getOpaquePointer(s2)));
// CHECK-NEXT: }


// CHECK: inline StructOneI64 passThroughStructOneI64(const StructOneI64& x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructOneI64::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructOneI64(result, _impl::$s7Structs23passThroughStructOneI64yAA0deF0VADF(_impl::swift_interop_passDirect_Structs_StructOneI64(_impl::_impl_StructOneI64::getOpaquePointer(x))));
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline StructTwoI32 passThroughStructTwoI32(int32_t i, const StructTwoI32& x, int32_t j) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructTwoI32::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructTwoI32(result, _impl::$s7Structs23passThroughStructTwoI32yAA0deF0Vs5Int32V_AdFtF(i, _impl::swift_interop_passDirect_Structs_StructTwoI32(_impl::_impl_StructTwoI32::getOpaquePointer(x)), j));
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline void printStructOneI64(const StructOneI64& x) noexcept {
// CHECK-NEXT:  return _impl::$s7Structs17printStructOneI64yyAA0cdE0VF(_impl::swift_interop_passDirect_Structs_StructOneI64(_impl::_impl_StructOneI64::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline void printStructStructTwoI32_and_OneI16AndOneStruct(const StructTwoI32& y, const StructOneI16AndOneStruct& x) noexcept {
// CHECK-NEXT:  return _impl::$s7Structs011printStructc20TwoI32_and_OneI16AndgC0yyAA0cdE0V_AA0cghigC0VtF(_impl::swift_interop_passDirect_Structs_StructTwoI32(_impl::_impl_StructTwoI32::getOpaquePointer(y)), _impl::swift_interop_passDirect_Structs_StructOneI16AndOneStruct(_impl::_impl_StructOneI16AndOneStruct::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline void printStructTwoI32(const StructTwoI32& x) noexcept {
// CHECK-NEXT:  return _impl::$s7Structs17printStructTwoI32yyAA0cdE0VF(_impl::swift_interop_passDirect_Structs_StructTwoI32(_impl::_impl_StructTwoI32::getOpaquePointer(x)));
// CHECK-NEXT: }


// CHECK: inline StructDoubleAndFloat returnNewStructDoubleAndFloat(float y, double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructDoubleAndFloat::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructDoubleAndFloat(result, _impl::$s7Structs29returnNewStructDoubleAndFloatyAA0defG0VSf_SdtF(y, x));
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline StructOneI16AndOneStruct returnNewStructOneI16AndOneStruct() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructOneI16AndOneStruct::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructOneI16AndOneStruct(result, _impl::$s7Structs024returnNewStructOneI16AndeD0AA0defgeD0VyF());
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline StructOneI64 returnNewStructOneI64() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructOneI64::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructOneI64(result, _impl::$s7Structs21returnNewStructOneI64AA0deF0VyF());
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline StructTwoI32 returnNewStructTwoI32(int32_t x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructTwoI32::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructTwoI32(result, _impl::$s7Structs21returnNewStructTwoI32yAA0deF0Vs5Int32VF(x));
// CHECK-NEXT:  });
// CHECK-NEXT: }


// CHECK: inline StructU16AndPointer returnNewStructU16AndPointer(void * _Nonnull x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_StructU16AndPointer::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:    _impl::swift_interop_returnDirect_Structs_StructU16AndPointer(result, _impl::$s7Structs28returnNewStructU16AndPointeryAA0defG0VSvF(x));
// CHECK-NEXT:  });
// CHECK-NEXT: }
