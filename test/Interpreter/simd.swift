// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import simd

public struct Vector3f {
  var f3: float3
  init() {
    f3 = float3(0, 1, 2)
  }
}

public struct TwoFloat {
  var f0 : Float
  var f1 : Float
  init() {
    f0 = 0.0
    f1 = 1.0
  }
}

public func test() -> (Vector3f, TwoFloat) {
  let v = Vector3f()
  let y = TwoFloat()
  let r = (v, y)
  return r
}

// CHECK: (main.Vector3f(f3: SIMD3<Float>(0.0, 1.0, 2.0)), main.TwoFloat(f0: 0.0, f1: 1.0))
print(test())
