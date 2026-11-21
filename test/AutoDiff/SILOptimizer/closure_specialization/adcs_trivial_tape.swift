// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name trivial_tape %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.log
// RUN: %FileCheck %s --check-prefix=AFTER < %t.log
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// BEFORE-LABEL: {{^}}// pullback of tupleBranch(_:_:)
// BEFORE:       bb0(%[[#]] : $Float, %[[#]] : $Float, %[[#TAPE:]] : @owned $_AD__{{.*}}):
// BEFORE-NEXT:    destroy_value %[[#TAPE]]
// BEFORE:         return
// BEFORE-NEXT:  } // end sil function

// AFTER-LABEL: {{^}}// specialized pullback of tupleBranch(_:_:)
// AFTER:       // specialized pullback of tupleBranch(_:_:)
// AFTER:       bb0(%[[#]] : $Float, %[[#]] : $Float, %[[#]] : $_AD__{{.*}}_spec{{.*}}):
// AFTER-NOT:     destroy_value
// AFTER:         return
// AFTER-NEXT:  } // end sil function

// FINAL-LABEL: {{^}}// specialized pullback of tupleBranch(_:_:)
// FINAL:       sil private [signature_optimized_thunk] [heuristic_always_inline] {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float) {
// FINAL-NOT:     destroy_value
// FINAL:         return
// FINAL-NEXT:  } // end sil function

import _Differentiation

// The second component's pullback only destroys its tape. Replacing the
// saved functions with Float captures makes that tape trivial.
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
