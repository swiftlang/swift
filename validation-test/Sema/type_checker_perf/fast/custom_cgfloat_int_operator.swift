// RUN: %target-typecheck-verify-swift -solver-scope-threshold=4000 -solver-enable-prune-disjunctions

// REQUIRES: objc_interop

import Foundation

func * (int: Int, float: CGFloat) -> CGFloat { fatalError() }

func g(_: CGFloat, _: CGFloat) {}
func g(_: Double, _: Double) {}

func f(x: Int, y: CGFloat) {
  g(y - y / 2 - (x * 10), -y - 15 + (x * 10))
}
