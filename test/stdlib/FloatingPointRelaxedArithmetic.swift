//===--- FloatingPointRelaxedArithmetic.swift -----------------*- swift -*-===//
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
// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu

import Swift

#if arch(arm64)
@available(macOS 11.0, iOS 14.0, tvOS 14.0, watchOS 7.0, *)
func float16_mulAdd(_ a: Float16, _ b: Float16, _ c: Float16) -> Float16 {
  a &+ (b &* c)
}

// CHECK-arm64: float16_mulAdd
// CHECK-arm64: fmul reassoc nsz contract half
// CHECK-arm64: fadd reassoc nsz contract half

@available(macOS 11.0, iOS 14.0, tvOS 14.0, watchOS 7.0, *)
func float16_mulSub(_ a: Float16, _ b: Float16, _ c: Float16) -> Float16 {
  a &- (b &* c)
}

// CHECK-arm64: float16_mulSub
// CHECK-arm64: fmul reassoc nsz contract half
// CHECK-arm64: fsub reassoc nsz contract half
#endif

func float_mulAdd(_ a: Float, _ b: Float, _ c: Float) -> Float {
  a &+ (b &* c)
}

// CHECK: float_mulAdd
// CHECK: fmul reassoc nsz contract float
// CHECK: fadd reassoc nsz contract float

func float_mulSub(_ a: Float, _ b: Float, _ c: Float) -> Float {
  a &- (b &* c)
}

// CHECK: float_mulSub
// CHECK: fmul reassoc nsz contract float
// CHECK: fsub reassoc nsz contract float

func double_mulAdd(_ a: Double, _ b: Double, _ c: Double) -> Double {
  a &+ (b &* c)
}

// CHECK: double_mulAdd
// CHECK: fmul reassoc nsz contract double
// CHECK: fadd reassoc nsz contract double

func double_mulSub(_ a: Double, _ b: Double, _ c: Double) -> Double {
  a &- (b &* c)
}

// CHECK: double_mulSub
// CHECK: fmul reassoc nsz contract double
// CHECK: fsub reassoc nsz contract double
