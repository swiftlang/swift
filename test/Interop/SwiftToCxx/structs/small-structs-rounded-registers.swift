//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h
// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -enable-library-evolution -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/frozen-structs.h
// RUN: %FileCheck %s < %t/frozen-structs.h

// REQUIRES: PTRSIZE=64

// The copy ranges, and therefore the stub names, must distinguish Swift values
// that use the same register types but have different storage sizes (#93614).
// CHECK-DAG: swift_interop_returnDirect_Structs_uint32_t_0_3(
// CHECK-DAG: swift_interop_passDirect_Structs_uint32_t_0_3(
// CHECK-DAG: swift_interop_returnDirect_Structs_uint64_t_0_5(
// CHECK-DAG: swift_interop_passDirect_Structs_uint64_t_0_5(
// CHECK-DAG: swift_interop_returnDirect_Structs_uint64_t_0_8(
// CHECK-DAG: swift_interop_passDirect_Structs_uint64_t_0_8(
// CHECK-DAG: swift_interop_returnDirect_Structs_uint64_t_0_8_uint32_t_8_12(
// CHECK-DAG: swift_interop_passDirect_Structs_uint64_t_0_8_uint32_t_8_12(
// CHECK-DAG: swift_interop_returnDirect_Structs_uint64_t_0_8_uint64_t_8_13(
// CHECK-DAG: swift_interop_passDirect_Structs_uint64_t_0_8_uint64_t_8_13(
// CHECK-DAG: swift_interop_returnDirect_Structs_uint64_t_0_8_uint64_t_8_16(
// CHECK-DAG: swift_interop_passDirect_Structs_uint64_t_0_8_uint64_t_8_16(

@frozen public struct S3 {
  public var a, b, c: Bool?
  public init() { a = true; c = false }
}
public func makeS3() -> S3 { S3() }
public func takeS3(_ s: S3) -> Bool { s.a == true && s.b == nil && s.c == false }

@frozen public struct S5 {
  public var a, b, c, d, e: Bool?
  public init() { a = true; e = false }
}
public func makeS5() -> S5 { S5() }
public func takeS5(_ s: S5) -> Bool { s.a == true && s.c == nil && s.e == false }

@frozen public struct S8 {
  public var a, b, c, d, e, f, g, h: Bool?
  public init() { a = true; h = false }
}
public func makeS8() -> S8 { S8() }
public func takeS8(_ s: S8) -> Bool { s.a == true && s.d == nil && s.h == false }

@frozen public struct S12 {
  public var a, b, c, d, e, f, g, h, i, j, k, l: Bool?
  public init() { a = true; l = false }
}
public func makeS12() -> S12 { S12() }
public func takeS12(_ s: S12) -> Bool { s.a == true && s.f == nil && s.l == false }

@frozen public struct S13 {
  public var a, b, c, d, e, f, g, h, i, j, k, l, m: Bool?
  public init() { a = true; m = false }
}
public func makeS13() -> S13 { S13() }
public func takeS13(_ s: S13) -> Bool { s.a == true && s.g == nil && s.m == false }

@frozen public struct S16 {
  public var a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p: Bool?
  public init() { a = true; p = false }
}
public func makeS16() -> S16 { S16() }
public func takeS16(_ s: S16) -> Bool { s.a == true && s.h == nil && s.p == false }

@frozen public struct DoubleAndBool {
  public var value: Double = 42.5
  public var flag: Bool? = false
  public init() {}
}
public func makeDoubleAndBool() -> DoubleAndBool { DoubleAndBool() }
public func takeDoubleAndBool(_ s: DoubleAndBool) -> Bool {
  s.value == 42.5 && s.flag == false
}
