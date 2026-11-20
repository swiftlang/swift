// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil %s | %FileCheck %s

import _Differentiation

@differentiable(reverse)
func square(_ x: inout Float) {
  x *= x
}

@differentiable(reverse)
func twoSteps(_ x: inout Float) {
  square(&x)
  square(&x)
}

@differentiable(reverse)
func fourSteps(_ x: inout Float) {
  twoSteps(&x)
  twoSteps(&x)
}

// Specialize fourSteps' leaf callbacks before inlining its VJP into the caller.
@differentiable(reverse, wrt: x)
public func branchTwoCalls(_ x: inout Float, _ square: Bool) {
  fourSteps(&x)
  if square { x *= x }
  fourSteps(&x)
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of branchTwoCalls(_:_:)
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of branchTwoCalls(_:_:)
// CHECK-NEXT:    %[[#FN:]] = function_ref
// CHECK-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#FN]](
// CHECK-NEXT:    return %[[#PB]]
// CHECK-NOT:     = partial_apply
// CHECK:       } // end sil function
