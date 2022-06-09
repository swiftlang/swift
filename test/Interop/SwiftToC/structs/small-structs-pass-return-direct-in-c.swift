// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-c-header-in-clang(%t/structs.h -Wno-unused-function)

public struct StructOneI64 {
    let x: Int64
}

public struct StructTwoI32 {
    let x, y: Int32
}

public struct StructOneI16AndOneStruct {
    let x: Int16
    let y: StructTwoI32
}

public struct StructU16AndPointer {
    let x: UInt8
    let y: UnsafeMutableRawPointer
}

public struct StructDoubleAndFloat {
    let x: Double
    let y: Float
}

public struct StructI8AndU32AndI16 {
    let x: Int8
    let y: UInt32
    let z: Int16
}

// CHECK:      struct Structs_StructDoubleAndFloat {
// CHECK_NEXT:   _Alignas(8) char _storage[12];
// CHECK_NEXT: };

// CHECK:      struct Structs_StructI8AndU32AndI16 {
// CHECK_NEXT:   _Alignas(4) char _storage[10];
// CHECK_NEXT: };

// CHECK:      struct Structs_StructOneI16AndOneStruct {
// CHECK_NEXT:   _Alignas(4) char _storage[12];
// CHECK_NEXT: };

// CHECK:      struct Structs_StructOneI64 {
// CHECK_NEXT:   _Alignas(8) char _storage[8];
// CHECK_NEXT: };

// CHECK:      struct Structs_StructTwoI32 {
// CHECK_NEXT:   _Alignas(4) char _storage[8];
// CHECK_NEXT: };

// CHECK:      struct Structs_StructU16AndPointer {
// CHECK_NEXT:   _Alignas(8) char _storage[16];
// CHECK_NEXT: };

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

public func returnNewStructI8AndU32AndI16() -> StructI8AndU32AndI16 {
    return StructI8AndU32AndI16(x: -100, y: 123456, z: -3456)
}

public func printStructI8AndU32AndI16(_ x: StructI8AndU32AndI16) {
    print("StructI8AndU32AndI16.x = \(x.x), y = \(x.y), z = \(x.z)")
}

// CHECK:      struct swift_interop_stub_Structs_StructDoubleAndFloat {
// CHECK-NEXT:  double _1;
// CHECK-NEXT:  float _2;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructDoubleAndFloat(char * _Nonnull result, struct swift_interop_stub_Structs_StructDoubleAndFloat value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:  memcpy(result + 8, &value._2, 4);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructDoubleAndFloat swift_interop_passDirect_Structs_StructDoubleAndFloat(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructDoubleAndFloat result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  memcpy(&result._2, value + 8, 4);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN double $s7Structs25getStructDoubleAndFloat_xySdAA0cdeF0VF(struct swift_interop_stub_Structs_StructDoubleAndFloat x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN float $s7Structs25getStructDoubleAndFloat_yySfAA0cdeF0VF(struct swift_interop_stub_Structs_StructDoubleAndFloat x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_stub_Structs_StructU16AndPointer {
// CHECK-NEXT:  uint8_t _1;
// CHECK-NEXT:  void * _Null_unspecified _2;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructU16AndPointer(char * _Nonnull result, struct swift_interop_stub_Structs_StructU16AndPointer value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 1);
// CHECK-NEXT:  memcpy(result + [[PTRSIZE:[48]]], &value._2, [[PTRSIZE]]);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructU16AndPointer swift_interop_passDirect_Structs_StructU16AndPointer(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructU16AndPointer result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 1);
// CHECK-NEXT:  memcpy(&result._2, value + [[PTRSIZE]], [[PTRSIZE]]);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN uint8_t $s7Structs24getStructU16AndPointer_xys5UInt8VAA0cdeF0VF(struct swift_interop_stub_Structs_StructU16AndPointer x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN void * _Nonnull $s7Structs24getStructU16AndPointer_yySvAA0cdeF0VF(struct swift_interop_stub_Structs_StructU16AndPointer x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_stub_Structs_StructOneI64 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructOneI64(char * _Nonnull result, struct swift_interop_stub_Structs_StructOneI64 value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructOneI64 swift_interop_passDirect_Structs_StructOneI64(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructOneI64 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructOneI64 $s7Structs23passThroughStructOneI64yAA0deF0VADF(struct swift_interop_stub_Structs_StructOneI64 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_stub_Structs_StructTwoI32 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructTwoI32(char * _Nonnull result, struct swift_interop_stub_Structs_StructTwoI32 value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructTwoI32 swift_interop_passDirect_Structs_StructTwoI32(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructTwoI32 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructTwoI32 $s7Structs23passThroughStructTwoI32yAA0deF0Vs5Int32V_AdFtF(int32_t i, struct swift_interop_stub_Structs_StructTwoI32 x, int32_t j) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_stub_Structs_StructI8AndU32AndI16 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT:  uint16_t _2;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructI8AndU32AndI16(char * _Nonnull result, struct swift_interop_stub_Structs_StructI8AndU32AndI16 value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:  memcpy(result + 8, &value._2, 2);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructI8AndU32AndI16 swift_interop_passDirect_Structs_StructI8AndU32AndI16(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructI8AndU32AndI16 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  memcpy(&result._2, value + 8, 2);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN void $s7Structs019printStructI8AndU32E3I16yyAA0cdefeG0VF(struct swift_interop_stub_Structs_StructI8AndU32AndI16 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN void $s7Structs17printStructOneI64yyAA0cdE0VF(struct swift_interop_stub_Structs_StructOneI64 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_stub_Structs_StructOneI16AndOneStruct {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT:  uint32_t _2;
// CHECK-NEXT: };

// CHECK:      static inline void swift_interop_returnDirect_Structs_StructOneI16AndOneStruct(char * _Nonnull result, struct swift_interop_stub_Structs_StructOneI16AndOneStruct value) __attribute__((always_inline)) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:  memcpy(result + 8, &value._2, 4);
// CHECK-NEXT: }

// CHECK:      static inline struct swift_interop_stub_Structs_StructOneI16AndOneStruct swift_interop_passDirect_Structs_StructOneI16AndOneStruct(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:  struct swift_interop_stub_Structs_StructOneI16AndOneStruct result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  memcpy(&result._2, value + 8, 4);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN void $s7Structs011printStructc20TwoI32_and_OneI16AndgC0yyAA0cdE0V_AA0cghigC0VtF(struct swift_interop_stub_Structs_StructTwoI32 y, struct swift_interop_stub_Structs_StructOneI16AndOneStruct x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN void $s7Structs17printStructTwoI32yyAA0cdE0VF(struct swift_interop_stub_Structs_StructTwoI32 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructDoubleAndFloat $s7Structs29returnNewStructDoubleAndFloatyAA0defG0VSf_SdtF(float y, double x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructI8AndU32AndI16 $s7Structs023returnNewStructI8AndU32F3I16AA0defgfH0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructOneI16AndOneStruct $s7Structs024returnNewStructOneI16AndeD0AA0defgeD0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructOneI64 $s7Structs21returnNewStructOneI64AA0deF0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructTwoI32 $s7Structs21returnNewStructTwoI32yAA0deF0Vs5Int32VF(int32_t x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_stub_Structs_StructU16AndPointer $s7Structs28returnNewStructU16AndPointeryAA0defG0VSvF(void * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
