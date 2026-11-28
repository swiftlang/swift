// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck --check-prefix=CHECK-%target-ptrsize %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// The tuple is 8-aligned, so `t.0` is at offset 8.
public struct StructBoolAndPair {
    public var a: Bool
    public var t: (Int16, Int64)
}

// The tuple is 4-aligned, so `t.0` is at offset 8, in a different
// pointer-sized chunk than `b`.
public struct StructFloatBoolAndPair {
    public var a: Float
    public var b: Bool
    public var t: (Int16, Float)
}

public struct Inner {
    public var x: Int8
    public var y: Int64
}

// `b.x` is at offset 8, in a different pointer-sized chunk than `a`.
public struct Outer {
    public var a: Int8
    public var b: Inner
}

// Two consecutive fields need to be moved to the next chunk.
public struct OuterCascade {
    public var a: Int8
    public var b: Outer
}

public func returnNewStructBoolAndPair() -> StructBoolAndPair {
    return StructBoolAndPair(a: true, t: (-2, 0x1122334455667788))
}

public func passThroughStructBoolAndPair(_ x: StructBoolAndPair) -> StructBoolAndPair { return x }

public func printStructBoolAndPair(_ x: StructBoolAndPair) {
    print("StructBoolAndPair(\(x.a), \(x.t.0), \(x.t.1))")
}

public func returnNewStructFloatBoolAndPair() -> StructFloatBoolAndPair {
    return StructFloatBoolAndPair(a: 1.5, b: true, t: (-2, 2.5))
}

public func passThroughStructFloatBoolAndPair(_ x: StructFloatBoolAndPair) -> StructFloatBoolAndPair { return x }

public func printStructFloatBoolAndPair(_ x: StructFloatBoolAndPair) {
    print("StructFloatBoolAndPair(\(x.a), \(x.b), \(x.t.0), \(x.t.1))")
}

public func returnNewOuter() -> Outer {
    return Outer(a: 0x7f, b: Inner(x: 0x55, y: -3))
}

public func passThroughOuter(_ x: Outer) -> Outer { return x }

public func printOuter(_ x: Outer) {
    print("Outer(\(x.a), Inner(\(x.b.x), \(x.b.y)))")
}

public func returnNewOuterCascade() -> OuterCascade {
    return OuterCascade(a: 1, b: Outer(a: 2, b: Inner(x: 3, y: -4)))
}

public func passThroughOuterCascade(_ x: OuterCascade) -> OuterCascade { return x }

public func printOuterCascade(_ x: OuterCascade) {
    print("OuterCascade(\(x.a), \(x.b.a), \(x.b.b.x), \(x.b.b.y))")
}

// OuterCascade
// CHECK-64:      struct swift_interop_passStub_Structs_uint8_t_0_1_uint8_t_8_9_uint8_t_16_17_uint64_t_24_32 {
// CHECK-64-NEXT:   uint8_t _1;
// CHECK-64-NEXT:   uint8_t _2 __attribute__((aligned(8)));
// CHECK-64-NEXT:   uint8_t _3 __attribute__((aligned(8)));
// CHECK-64-NEXT:   uint64_t _4;
// CHECK-64-NEXT: };

// StructBoolAndPair
// CHECK-64:      struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 {
// CHECK-64-NEXT:   bool _1;
// CHECK-64-NEXT:   uint16_t _2 __attribute__((aligned(8)));
// CHECK-64-NEXT:   uint64_t _3;
// CHECK-64-NEXT: };
// CHECK-64-EMPTY:
// CHECK-64-NEXT: static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 swift_interop_passDirect_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24(const char * _Nonnull value) {
// CHECK-64-NEXT:   struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 result;
// CHECK-64-NEXT:   memcpy(&result._1, value + 0, 1);
// CHECK-64-NEXT:   memcpy(&result._2, value + 8, 2);
// CHECK-64-NEXT:   memcpy(&result._3, value + 16, 8);

// CHECK-32:      struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 {
// CHECK-32-NEXT:   bool _1;
// CHECK-32-NEXT:   uint16_t _2 __attribute__((aligned(4)));
// CHECK-32-NEXT:   uint64_t _3 __attribute__((aligned(8)));
// CHECK-32-NEXT: };
// CHECK-32-EMPTY:
// CHECK-32-NEXT: static SWIFT_C_INLINE_THUNK struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 swift_interop_passDirect_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24(const char * _Nonnull value) {
// CHECK-32-NEXT:   struct swift_interop_passStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 result;
// CHECK-32-NEXT:   memcpy(&result._1, value + 0, 1);
// CHECK-32-NEXT:   memcpy(&result._2, value + 8, 2);
// CHECK-32-NEXT:   memcpy(&result._3, value + 16, 8);

// StructFloatBoolAndPair
// CHECK-64:      struct swift_interop_passStub_Structs_float_0_4_bool_4_5_uint16_t_8_10_float_12_16 {
// CHECK-64-NEXT:   float _1;
// CHECK-64-NEXT:   bool _2;
// CHECK-64-NEXT:   uint16_t _3 __attribute__((aligned(8)));
// CHECK-64-NEXT:   float _4;
// CHECK-64-NEXT: };

// CHECK-32:      struct swift_interop_passStub_Structs_float_0_4_bool_4_5_uint16_t_8_10_float_12_16 {
// CHECK-32-NEXT:   float _1;
// CHECK-32-NEXT:   bool _2;
// CHECK-32-NEXT:   uint16_t _3 __attribute__((aligned(4)));
// CHECK-32-NEXT:   float _4;
// CHECK-32-NEXT: };

// StructBoolAndPair
// CHECK-64:      struct swift_interop_returnStub_Structs_bool_0_1_uint16_t_8_10_uint64_t_16_24 {
// CHECK-64-NEXT:   bool _1;
// CHECK-64-NEXT:   uint16_t _2 __attribute__((aligned(8)));
// CHECK-64-NEXT:   uint64_t _3;
// CHECK-64-NEXT: };
