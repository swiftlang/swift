// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50
// REQUIRES: OS=macosx

// https://github.com/swiftlang/swift/issues/48033

import Foundation

func bar() -> Float {
  return (.pi / 2.0 - 2.0 * atan(exp((round(2.0))))) * 180.0 / .pi
}
