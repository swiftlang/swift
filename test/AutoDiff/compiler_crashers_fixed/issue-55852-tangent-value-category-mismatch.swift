// RUN: %target-build-swift %s

// https://github.com/apple/swift/issues/55852
// Semantic member getter pullback generation crash due to tangent value
// category mismatch

// Sometimes the linker crashes on linux
// UNSUPPORTED: OS=linux-gnu

import _Differentiation

struct Dense: Differentiable {
  @differentiable(reverse)
  var bias: Float?
}
