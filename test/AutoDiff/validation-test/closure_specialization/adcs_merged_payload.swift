// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name ADCSMP %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

// The pullback merges payloads from different predecessor enums for the
// shared addition and subtraction.
// CHECK-LABEL: {{^}}// reverse-mode derivative of mergedPayload(_:_:)
// CHECK-NOT:     } // end sil function
// CHECK:         // function_ref specialized pullback of mergedPayload(_:_:)
// CHECK-NEXT:    %[[#PULLBACK:]] = function_ref
// CHECK-NEXT:    %[[#BOUND:]] = partial_apply {{.*}} %[[#PULLBACK]](
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#BOUND]])
// CHECK-NEXT:    return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function
// CHECK-LABEL: {{^}}// specialized pullback of mergedPayload(_:_:)
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
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
