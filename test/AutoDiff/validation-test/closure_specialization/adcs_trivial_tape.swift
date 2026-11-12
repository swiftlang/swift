// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

import _Differentiation

// Tuple-result derivative lowering
// leaves a pullback which only destroys its tape. Replacing the taped functions
// with Float captures makes the entire tape trivial, so that destroy must be
// removed by the type-changing AutoDiff rewrite.
@differentiable(reverse)
@inline(never)
public func tupleBranch(_ x: Float, _ y: Float) -> (Float, Float) {
  if x < 0 {
    if y > 0 { return (3 * y * y, 1) }
    return (0, 1)
  }
  if y > 0 { return (4 * y * y * y, 6) }
  return (0, 6)
}

// A scalar projection lets the runtime test call the generated derivative
// through the ordinary Float valueWithPullback API.
@differentiable(reverse)
@inline(never)
public func secondComponent(_ x: Float, _ y: Float) -> Float {
  tupleBranch(x, y).1
}

#if !DEFINITIONS_ONLY
for (x, y, first, second): (Float, Float, Float, Float) in [
  (-1, 2, 12, 1), (-1, 0, 0, 1), (1, 2, 32, 6), (1, 0, 0, 6)
] {
  let result = tupleBranch(x, y)
  precondition(result.0 == first)
  precondition(result.1 == second)
  let (value, pullback) = valueWithPullback(at: x, y, of: secondComponent)
  let (dx, dy) = pullback(1)
  precondition(value == second)
  precondition(dx == 0 && dy == 0)
}
#endif
