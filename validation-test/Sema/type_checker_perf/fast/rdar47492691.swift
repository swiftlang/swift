// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: objc_interop,no_asan

import CoreGraphics
import simd

func test(foo: CGFloat, bar: CGFloat) {
  _ = CGRect(x: 0.0 + 1.0, y: 0.0 + foo, width: 3.0 - 1 - 1 - 1.0, height: bar)
}
