// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// REQUIRES: objc_interop

import simd

func slow() {
  let ex = SIMD2<Double>(1.0, 2.0)
  let ey = SIMD2<Double>(1.0, 2.0)
  let _ = [ex+ey, ex-ey, -ex+ey, -ex-ey]
}

