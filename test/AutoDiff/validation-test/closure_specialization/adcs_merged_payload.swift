// REQUIRES: executable_test
// RUN: %target-build-swift %s -Onone -o %t.none
// RUN: %target-run %t.none
// RUN: %target-build-swift %s -O -o %t.opt
// RUN: %target-run %t.opt

import _Differentiation

// The two paths save the same original basic block's payload in different
// predecessor enums. The pullback joins those payloads before applying the
// closures of the shared addition and subtraction.
@differentiable(reverse)
public func mergedPayload(_ x: Float, _ y: Float) -> Float {
  let sum = x + y
  let difference = x - y
  let selected: Float
  if x < y {
    selected = sum + y
  } else {
    selected = difference - y
  }
  let shifted = selected + x
  return x > 0 ? shifted * x : shifted + y
}

// OPT-LABEL: // reverse-mode derivative of mergedPayload(_:_:)
// OPT: function_ref specialized pullback of mergedPayload(_:_:)
// OPT: = partial_apply
// OPT-NOT: = partial_apply
// OPT: return
// OPT-LABEL: // specialized pullback of mergedPayload(_:_:)
// OPT-NOT: @callee_guaranteed
// OPT: } // end sil function

func check(_ x: Float, _ y: Float, _ value: Float, _ dx: Float, _ dy: Float) {
  let result = valueWithGradient(at: x, y, of: mergedPayload)
  precondition(result.value == value)
  precondition(result.gradient.0 == dx)
  precondition(result.gradient.1 == dy)
}

check(1, 2, 6, 8, 2)
check(3, 2, 6, 8, -6)
check(-1, 2, 4, 2, 3)
check(-1, -2, 0, 2, -1)
