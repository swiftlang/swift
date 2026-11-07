//===--- SIMDReplace.swift ------------------------------------*- swift -*-===//
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
// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// The CHECK-SAME lines below look for a 128-bit vector passed as a direct
// parameter. Require architectures that use that convention.
// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=aarch64 || CPU=x86_64

// Check that concrete 128-bit vector replacement operations are lowered to
// a vector select, rather than a loop.

import Swift

func replacing_16xUInt8(
  _ a: SIMD16<UInt8>, _ b: SIMD16<UInt8>, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMD16<UInt8> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_16xUInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], <16 x i8> [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[B]], <16 x i8> [[A]]

func replacing_8xInt16(
  _ a: SIMD8<Int16>, _ b: SIMD8<Int16>, _ m: SIMDMask<SIMD8<Int16>>
) -> SIMD8<Int16> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_8xInt16
// CHECK-SAME: (<8 x i16> [[A:%[0-9]+]], <8 x i16> [[B:%[0-9]+]], <8 x i16> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <8 x i16> [[M]], zeroinitializer
// CHECK: select <8 x i1> [[P]], <8 x i16> [[B]], <8 x i16> [[A]]

func replacing_2xInt64(
  _ a: SIMD2<Int64>, _ b: SIMD2<Int64>, _ m: SIMDMask<SIMD2<Int64>>
) -> SIMD2<Int64> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_2xInt64
// CHECK-SAME: (<2 x i64> [[A:%[0-9]+]], <2 x i64> [[B:%[0-9]+]], <2 x i64> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <2 x i64> [[M]], zeroinitializer
// CHECK: select <2 x i1> [[P]], <2 x i64> [[B]], <2 x i64> [[A]]

func replacing_4xFloat(
  _ a: SIMD4<Float>, _ b: SIMD4<Float>, _ m: SIMDMask<SIMD4<Int32>>
) -> SIMD4<Float> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_4xFloat
// CHECK-SAME: (<4 x float> [[A:%[0-9]+]], <4 x float> [[B:%[0-9]+]], <4 x i32> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <4 x i32> [[M]], zeroinitializer
// CHECK: select <4 x i1> [[P]], <4 x float> [[B]], <4 x float> [[A]]

func replacing_2xDouble(
  _ a: SIMD2<Double>, _ b: SIMD2<Double>, _ m: SIMDMask<SIMD2<Int64>>
) -> SIMD2<Double> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_2xDouble
// CHECK-SAME: (<2 x double> [[A:%[0-9]+]], <2 x double> [[B:%[0-9]+]], <2 x i64> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <2 x i64> [[M]], zeroinitializer
// CHECK: select <2 x i1> [[P]], <2 x double> [[B]], <2 x double> [[A]]

// SIMD3 is stored in 4 lanes; this underlying operation is over 128 bits.
func replacing_3xFloat(
  _ a: SIMD3<Float>, _ b: SIMD3<Float>, _ m: SIMDMask<SIMD3<Int32>>
) -> SIMD3<Float> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_3xFloat
// CHECK-SAME: (<4 x float> [[A:%[0-9]+]], <4 x float> [[B:%[0-9]+]], <4 x i32> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <4 x i32> [[M]], zeroinitializer
// CHECK: select <4 x i1> [[P]], <4 x float> [[B]], <4 x float> [[A]]

func replacing_scalar_16xUInt8(
  _ a: SIMD16<UInt8>, _ b: UInt8, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMD16<UInt8> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_scalar_16xUInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], i8 [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[S:%[0-9]+]] = insertelement <16 x i8> zeroinitializer, i8 [[B]], i32 0
// CHECK: [[V:%[0-9]+]] = shufflevector <16 x i8> [[S]], <16 x i8> zeroinitializer
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[V]], <16 x i8> [[A]]

func replace_16xUInt8(
  _ a: SIMD16<UInt8>, _ b: SIMD16<UInt8>, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMD16<UInt8> {
  var result = a
  result.replace(with: b, where: m)
  return result
}
// CHECK-LABEL: define{{.*}}replace_16xUInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], <16 x i8> [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[B]], <16 x i8> [[A]]

func replace_scalar_16xUInt8(
  _ a: SIMD16<UInt8>, _ b: UInt8, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMD16<UInt8> {
  var result = a
  result.replace(with: b, where: m)
  return result
}
// CHECK-LABEL: define{{.*}}replace_scalar_16xUInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], i8 [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[S:%[0-9]+]] = insertelement <16 x i8> zeroinitializer, i8 [[B]], i32 0
// CHECK: [[V:%[0-9]+]] = shufflevector <16 x i8> [[S]], <16 x i8> zeroinitializer
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[V]], <16 x i8> [[A]]

func replacing_mask_16xInt8(
  _ a: SIMDMask<SIMD16<Int8>>,
  _ b: SIMDMask<SIMD16<Int8>>,
  _ m: SIMDMask<SIMD16<Int8>>
) -> SIMDMask<SIMD16<Int8>> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_mask_16xInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], <16 x i8> [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[B]], <16 x i8> [[A]]

func replacing_mask_bool_16xInt8(
  _ a: SIMDMask<SIMD16<Int8>>, _ b: Bool, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMDMask<SIMD16<Int8>> {
  a.replacing(with: b, where: m)
}
// CHECK-LABEL: define{{.*}}replacing_mask_bool_16xInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], i1 {{%[0-9]+}}, <16 x i8> [[M:%[0-9]+]])
// CHECK: [[V:%[0-9]+]] = shufflevector <16 x i8> {{.*}}
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[V]], <16 x i8> [[A]]

func replace_mask_16xInt8(
  _ a: SIMDMask<SIMD16<Int8>>,
  _ b: SIMDMask<SIMD16<Int8>>,
  _ m: SIMDMask<SIMD16<Int8>>
) -> SIMDMask<SIMD16<Int8>> {
  var result = a
  result.replace(with: b, where: m)
  return result
}
// CHECK-LABEL: define{{.*}}replace_mask_16xInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], <16 x i8> [[B:%[0-9]+]], <16 x i8> [[M:%[0-9]+]])
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[B]], <16 x i8> [[A]]

func replace_mask_bool_16xInt8(
  _ a: SIMDMask<SIMD16<Int8>>, _ b: Bool, _ m: SIMDMask<SIMD16<Int8>>
) -> SIMDMask<SIMD16<Int8>> {
  var result = a
  result.replace(with: b, where: m)
  return result
}
// CHECK-LABEL: define{{.*}}replace_mask_bool_16xInt8
// CHECK-SAME: (<16 x i8> [[A:%[0-9]+]], i1 {{%[0-9]+}}, <16 x i8> [[M:%[0-9]+]])
// CHECK: [[V:%[0-9]+]] = shufflevector <16 x i8> {{.*}}
// CHECK: [[P:%[0-9]+]] = icmp slt <16 x i8> [[M]], zeroinitializer
// CHECK: select <16 x i1> [[P]], <16 x i8> [[V]], <16 x i8> [[A]]
