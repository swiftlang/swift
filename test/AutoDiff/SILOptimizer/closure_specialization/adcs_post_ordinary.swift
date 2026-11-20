// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name out %s -Xllvm -sil-print-around=autodiff-closure-specialization -Xllvm -sil-print-functions=cond_tuple_var -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.passes

// CHECK-LABEL: {{^}}// reverse-mode derivative of cond_tuple_var(_:)
// CHECK:         // function_ref pullback of cond_tuple_var
// CHECK-NEXT:    %[[#ORIGINAL:]] = function_ref {{.*}} : $@convention(thin) (Float, @owned [[OLD_TAPE:_AD__[^, ]+]], @owned @callee_guaranteed
// CHECK-NEXT:  {{.*}} = partial_apply {{.*}} %[[#ORIGINAL]](%[[#]], %[[#]], %[[#]], %[[#]])

// The same visit that specializes the individual captures must rediscover
// the rewritten pullback to specialize its remaining tape.
// CHECK:      *** New SIL function in {{.*}}#[[#VISIT:]], stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: [[ORDINARY:[^ ]+]]
// CHECK:      sil {{.*}}@[[ORDINARY]] : $@convention(thin) (Float, @owned [[OLD_TAPE]]) -> Float
// CHECK:      *** New SIL function in {{.*}}#[[#VISIT]], stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: [[ORDINARY]][[SUFFIX:[^ ]+]]
// CHECK:      sil {{.*}}@[[ORDINARY]][[SUFFIX]] : $@convention(thin) (Float, @owned [[NEW_TAPE:_AD__[^ )]+_spec[^ )]+]]) -> Float
// CHECK:      *** SIL function after {{.*}}#[[#VISIT]], stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// CHECK-NEXT: // reverse-mode derivative of cond_tuple_var
// CHECK:        %[[#NEW_PB:]] = function_ref @[[ORDINARY]][[SUFFIX]]
// CHECK-NEXT:   %[[#BOUND_PB:]] = partial_apply {{.*}} %[[#NEW_PB]](%[[#]]) : $@convention(thin) (Float, @owned [[NEW_TAPE]]) -> Float
// CHECK-NEXT:   %[[#RESULT:]] = tuple (%[[#]], %[[#BOUND_PB]])
// CHECK-NEXT:   return %[[#RESULT]]

import _Differentiation

@differentiable(reverse)
public func cond_tuple_var(_ x: Float) -> Float {
  var y: (Float, Float) = (x, x)
  var z: (Float, Float) = (x + x, x - x)
  if x > 0 {
    let w = (x, x)
    y.0 = w.1
    y.1 = w.0
    z.0 = z.0 - y.0
    z.1 = z.1 + y.0
  } else {
    z = (1 * x, x)
  }
  return y.0 + y.1 - z.0 + z.1
}
