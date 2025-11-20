// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: objc_interop,no_asan

import CoreGraphics
import simd

func test(foo: CGFloat, bar: CGFloat) {
  _ = CGRect(x: 0.0 + 1.0, y: 0.0 + foo, width: 3.0 - 1 - 1 - 1.0, height: bar)
}

func test_with_generic_func_and_literals(bounds: CGRect) {
  _ = CGRect(x: 0, y: 0, width: 1, height: bounds.height - 2 + bounds.height / 2 + max(bounds.height / 2, bounds.height / 2))
}
