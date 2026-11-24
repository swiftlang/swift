// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50

// REQUIRES: objc_interop

import simd

var decelerationFactor: Float = 0.998

func initialVelocity_compilesSlowly(forTargetOffset targetOffset: float3, fromCurrentOffset currentOffset: float3) -> float3 { // expected-warning 3 {{'float3' is deprecated: Use SIMD3<Float>}}
    let k = decelerationFactor
    let v = (targetOffset - currentOffset) * (Float(1) - k) / k * Float(1000)
    return v
}
