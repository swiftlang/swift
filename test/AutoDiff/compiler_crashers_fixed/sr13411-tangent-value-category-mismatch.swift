// RUN: %target-build-swift %s
// REQUIRES: asserts

// SR-13411: Semantic member getter pullback generation crash due to tangent value category mismatch

import _Differentiation

struct Dense: Differentiable {
  @differentiable
  var bias: Float?
}

