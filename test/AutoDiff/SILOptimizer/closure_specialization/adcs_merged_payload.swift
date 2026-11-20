// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name ADCSMP %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=REWRITE < %t.passes
// RUN: %FileCheck %s --check-prefix=OPT < %t.sil

// INPUT-LABEL: {{^}}// pullback of mergedPayload(_:_:)
// INPUT:         %[[#LEFT:]] = unchecked_enum_data
// INPUT-NEXT:    br [[JOIN:bb[0-9]+]]({{.*}}, %[[#LEFT]])
// INPUT:         %[[#RIGHT:]] = unchecked_enum_data
// INPUT-NEXT:    br [[JOIN]]({{.*}}, %[[#RIGHT]])
// INPUT:       [[JOIN]]({{.*}}, %[[#PAYLOAD:]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> (Float, Float))):
// INPUT-NEXT:    (%[[#]], %[[#CALLBACK:]]) = destructure_tuple %[[#PAYLOAD]]
// INPUT:         %[[#]] = apply %[[#CALLBACK]]

// REWRITE-LABEL: {{^}}// specialized pullback of mergedPayload(_:_:)
// REWRITE:         %[[#LEFT:]] = unchecked_enum_data {{.*}}_spec
// REWRITE-NEXT:    br [[JOIN:bb[0-9]+]]({{.*}}, %[[#LEFT]])
// REWRITE:         %[[#RIGHT:]] = unchecked_enum_data {{.*}}_spec
// REWRITE-NEXT:    br [[JOIN]]({{.*}}, %[[#RIGHT]])
// REWRITE:       [[JOIN]]({{.*}}, %[[#PAYLOAD:]] : $(predecessor: {{.*}}_spec{{.*}}, ())):
// REWRITE-NEXT:    (%[[#]], %[[#]]) = destructure_tuple %[[#PAYLOAD]]

import _Differentiation

// Both paths save the shared block's payload in distinct predecessor enums.
// The pullback joins the whole payload before applying its closures.
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

// OPT-LABEL: {{^}}// reverse-mode derivative of mergedPayload(_:_:)
// OPT:         function_ref specialized pullback of mergedPayload(_:_:)
// OPT:         = partial_apply
// OPT-NOT:     = partial_apply
// OPT:         return
// OPT-LABEL: {{^}}// specialized pullback of mergedPayload(_:_:)
// OPT-NOT:   @callee_guaranteed
// OPT:       } // end sil function
