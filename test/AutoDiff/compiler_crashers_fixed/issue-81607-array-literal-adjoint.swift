// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/81607
// Ensure we're propagating array adjoint in the correct BB

import _Differentiation

@differentiable(reverse)
func sum(_ a: Float, _ b: [Float]) -> [Float] {
  if b.count != 0 {
    return b
  }
  return [a]
}
