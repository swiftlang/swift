// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import simd
import CoreGraphics

let m = simd_float3x3(1)

func foo(point: SIMD2<Float>, depth: Float, a: simd_float3x3? = nil) -> SIMD3<Float> {
  (a ?? m) * float3(point.x, point.y, 1) * depth
}
