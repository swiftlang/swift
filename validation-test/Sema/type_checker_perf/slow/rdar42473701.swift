// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// REQUIRES: objc_interop

import Foundation

// Invalid expression

func RightSideIn(Surface: CGSize, Slot: Int, Width: CGFloat, Height: CGFloat, RightMargin: CGFloat, BottomMargin: CGFloat) {
  let top = Surface.height - Height - BottomMargin - (((Slot + 1) * Height) + 10)
  // expected-error@-1 {{reasonable time}}
}
