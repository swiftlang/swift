// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: objc_interop,no_asan

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64 || CPU=arm64

import simd

func test(_: () -> Void) {}

test {
  let a: simd_float2 = .init()
  let b: simd_float2 = .init()
  let c: simd_float2 = .init()
  let d: simd_float2 = .init()

  let width: Float = 2

  let p = Int(max(20, min(1000, (simd_distance(a, b) + simd_distance(b, c) + simd_distance(c, d)) / width)))
  print(p)
}
