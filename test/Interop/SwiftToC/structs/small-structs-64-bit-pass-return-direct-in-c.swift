// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-c-header-in-clang(%t/structs.h -Wno-unused-function)

// REQUIRES: PTRSIZE=64

public struct StructTwoI32 {
    let x, y: Int32
}

public struct StructOneI16AndOneStruct {
    let x: Int16
    let y: StructTwoI32
}

public struct StructI8AndU32AndI16 {
    let x: Int8
    let y: UInt32
    let z: Int16
}

// CHECK:      struct Structs_StructI8AndU32AndI16 {
// CHECK-NEXT:   _Alignas(4) char _storage[10];
// CHECK-NEXT: };

// CHECK:      struct Structs_StructOneI16AndOneStruct {
// CHECK-NEXT:   _Alignas(4) char _storage[12];
// CHECK-NEXT: };

// CHECK:      struct Structs_StructTwoI32 {
// CHECK-NEXT:   _Alignas(4) char _storage[8];
// CHECK-NEXT: };

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

public func returnNewStructI8AndU32AndI16() -> StructI8AndU32AndI16 {
    return StructI8AndU32AndI16(x: -100, y: 123456, z: -3456)
}

public func printStructI8AndU32AndI16(_ x: StructI8AndU32AndI16) {
    print("StructI8AndU32AndI16.x = \(x.x), y = \(x.y), z = \(x.z)")
}

// CHECK:      struct swift_interop_returnStub_Structs_uint64_t_0_8 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK void swift_interop_returnDirect_Structs_uint64_t_0_8(char * _Nonnull result, struct swift_interop_returnStub_Structs_uint64_t_0_8 value) {
// CHECK-NEXT:  memcpy(result + 0, &value._1, 8);
// CHECK-NEXT: }

// CHECK:      struct swift_interop_passStub_Structs_uint64_t_0_8 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Structs_uint64_t_0_8 swift_interop_passDirect_Structs_uint64_t_0_8(const char * _Nonnull value) {
// CHECK-NEXT:  struct swift_interop_passStub_Structs_uint64_t_0_8 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Structs_uint64_t_0_8 $s7Structs23passThroughStructTwoI32yAA0deF0Vs5Int32V_AdFtF(int32_t i, struct swift_interop_passStub_Structs_uint64_t_0_8 x, int32_t j) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_passStub_Structs_uint64_t_0_8_uint16_t_8_10 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT:  uint16_t _2;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Structs_uint64_t_0_8_uint16_t_8_10 swift_interop_passDirect_Structs_uint64_t_0_8_uint16_t_8_10(const char * _Nonnull value) {
// CHECK-NEXT:  struct swift_interop_passStub_Structs_uint64_t_0_8_uint16_t_8_10 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  memcpy(&result._2, value + 8, 2);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN void $s7Structs019printStructI8AndU32E3I16yyAA0cdefeG0VF(struct swift_interop_passStub_Structs_uint64_t_0_8_uint16_t_8_10 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      struct swift_interop_passStub_Structs_uint64_t_0_8_uint32_t_8_12 {
// CHECK-NEXT:  uint64_t _1;
// CHECK-NEXT:  uint32_t _2;
// CHECK-NEXT: };

// CHECK:      static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Structs_uint64_t_0_8_uint32_t_8_12 swift_interop_passDirect_Structs_uint64_t_0_8_uint32_t_8_12(const char * _Nonnull value) {
// CHECK-NEXT:  struct swift_interop_passStub_Structs_uint64_t_0_8_uint32_t_8_12 result;
// CHECK-NEXT:  memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:  memcpy(&result._2, value + 8, 4);
// CHECK-NEXT:  return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN void $s7Structs011printStructc20TwoI32_and_OneI16AndgC0yyAA0cdE0V_AA0cghigC0VtF(struct swift_interop_passStub_Structs_uint64_t_0_8 y, struct swift_interop_passStub_Structs_uint64_t_0_8_uint32_t_8_12 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN void $s7Structs17printStructTwoI32yyAA0cdE0VF(struct swift_interop_passStub_Structs_uint64_t_0_8 x) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: struct swift_interop_returnStub_Structs_uint64_t_0_8_uint16_t_8_10 {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT:   uint16_t _2;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: static SWIFT_C_INLINE_THUNK void swift_interop_returnDirect_Structs_uint64_t_0_8_uint16_t_8_10(char * _Nonnull result, struct swift_interop_returnStub_Structs_uint64_t_0_8_uint16_t_8_10 value) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:   memcpy(result + 8, &value._2, 2);
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Structs_uint64_t_0_8_uint16_t_8_10 $s7Structs023returnNewStructI8AndU32F3I16AA0defgfH0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: struct swift_interop_returnStub_Structs_uint64_t_0_8_uint32_t_8_12 {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT:   uint32_t _2;
// CHECK-NEXT: };

// CHECK: static SWIFT_C_INLINE_THUNK void swift_interop_returnDirect_Structs_uint64_t_0_8_uint32_t_8_12(char * _Nonnull result, struct swift_interop_returnStub_Structs_uint64_t_0_8_uint32_t_8_12 value) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT:   memcpy(result + 8, &value._2, 4);
// CHECK-NEXT: }

// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Structs_uint64_t_0_8_uint32_t_8_12 $s7Structs024returnNewStructOneI16AndeD0AA0defgeD0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK:      SWIFT_EXTERN struct swift_interop_returnStub_Structs_uint64_t_0_8 $s7Structs21returnNewStructTwoI32yAA0deF0Vs5Int32VF(int32_t x) SWIFT_NOEXCEPT SWIFT_CALL;
