// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_trivial_tape %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK-LABEL: // reverse-mode derivative of tupleBranch(_:_:)
// CHECK:         // function_ref specialized pullback of tupleBranch(_:_:)
// CHECK-NEXT:    %[[#PULLBACK:]] = function_ref {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:    %[[#CLOSURE:]] = thin_to_thick_function %[[#PULLBACK]]
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#]], %[[#CLOSURE]])
// CHECK-NEXT:    return %[[#RESULT]]
// CHECK-LABEL: // specialized pullback of tupleBranch(_:_:)
// CHECK:       sil private {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float) {
// CHECK-NOT:     release_value
// CHECK:       } // end sil function

import _Differentiation

// This tuple-result pullback only destroys its branch tracing enum. Replacing
// the stored closures with Float captures makes the enum trivial.
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

@differentiable(reverse)
@inline(never)
public func secondComponent(_ x: Float, _ y: Float) -> Float {
  tupleBranch(x, y).1
}

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
