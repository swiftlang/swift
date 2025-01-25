// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1

// REQUIRES: OS=macosx,no_asan
// REQUIRES: objc_interop

import Foundation
import CoreGraphics
import simd

func test(
  a: CGFloat,
  b: CGFloat
) -> CGFloat {
  exp(-a * b) *
  (a * -sin(a * b) * a + ((a * b + a) / b) * cos(a * b) * a) +
  exp(-a * b) *
  (-b) *
  (a * cos(a * b) + ((a * b + a) / b) * sin(a * b))
}
