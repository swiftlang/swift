// RUN: %target-swift-frontend -emit-sil -verify %s

import _Differentiation

@differentiable(reverse)
func throwing(input: Float) throws -> Float {
  return input
}

@differentiable(reverse)
func catching(input: Float) -> Float {
  do {
    return try 1.0 * throwing(input: input)
  } catch {
    return 0.0
  }
}
