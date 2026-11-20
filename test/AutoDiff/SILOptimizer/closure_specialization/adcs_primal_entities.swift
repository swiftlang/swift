// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_primal_entities %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=GETTER < %t.passes
// RUN: %FileCheck %s --check-prefix=NESTED < %t.passes
// RUN: %FileCheck %s --check-prefix=GLOBAL < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// INPUT-LABEL: {{^}}// reverse-mode derivative of GetterCase.value.getter
// INPUT:         %[[#GETTER_CB:]] = partial_apply
// INPUT-NEXT:    %[[#GETTER_PAYLOAD:]] = tuple {{.*}}%[[#GETTER_CB]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#GETTER_PAYLOAD]]
// INPUT-LABEL: {{^}}// reverse-mode derivative of closure #1 in makeBranchClosure()
// INPUT:         %[[#NESTED_CB:]] = partial_apply
// INPUT-NEXT:    %[[#NESTED_PAYLOAD:]] = tuple {{.*}}%[[#NESTED_CB]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#NESTED_PAYLOAD]]
// INPUT-LABEL: {{^}}sil private [ossa] @$s20adcs_primal_entitiesS2fcfU_TJrSpSr :
// INPUT:         %[[#GLOBAL_CB:]] = partial_apply
// INPUT-NEXT:    %[[#GLOBAL_PAYLOAD:]] = tuple {{.*}}%[[#GLOBAL_CB]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#GLOBAL_PAYLOAD]]

// GETTER-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}GetterCaseV5valueSfvgTJp
// GETTER-NEXT:  // specialized pullback of GetterCase.value.getter
// GETTER:       // specialized pullback of GetterCase.value.getter
// GETTER-NOT:     = partial_apply
// GETTER-NOT:     apply {{.*}} : $@callee_guaranteed
// GETTER:         %[[#MULTIPLY:]] = function_ref @{{.*}}_vjpMultiply
// GETTER-NEXT:  {{.*}} = apply %[[#MULTIPLY]]({{.*}}) : $@convention(thin)
// GETTER-NOT:     = partial_apply
// GETTER-NOT:     apply {{.*}} : $@callee_guaranteed
// GETTER:       } // end sil function

// The enclosing named function is not the closure's primal entity.
// NESTED-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}makeBranchClosure{{.*}}cfU_TJp
// NESTED-NEXT:  // specialized pullback of closure #1 in makeBranchClosure()
// NESTED:       // specialized pullback of closure #1 in makeBranchClosure()
// NESTED-NOT:     = partial_apply
// NESTED-NOT:     apply {{.*}} : $@callee_guaranteed
// NESTED:         %[[#MULTIPLY:]] = function_ref @{{.*}}_vjpMultiply
// NESTED-NEXT:  {{.*}} = apply %[[#MULTIPLY]]({{.*}}) : $@convention(thin)
// NESTED-NOT:     = partial_apply
// NESTED-NOT:     apply {{.*}} : $@callee_guaranteed
// NESTED:       } // end sil function

// GLOBAL-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: $s20adcs_primal_entitiesS2fcfU_TJp
// GLOBAL-NEXT:  // specialized pullback of closure #1 in
// GLOBAL:       // specialized pullback of closure #1 in
// GLOBAL-NOT:     = partial_apply
// GLOBAL-NOT:     apply {{.*}} : $@callee_guaranteed
// GLOBAL:         %[[#MULTIPLY:]] = function_ref @{{.*}}_vjpMultiply
// GLOBAL-NEXT:  {{.*}} = apply %[[#MULTIPLY]]({{.*}}) : $@convention(thin)
// GLOBAL-NOT:     = partial_apply
// GLOBAL-NOT:     apply {{.*}} : $@callee_guaranteed
// GLOBAL:       } // end sil function

// FINAL-LABEL: {{^}}// reverse-mode derivative of GetterCase.value.getter
// FINAL-NOT:     = partial_apply
// FINAL:         // function_ref specialized pullback of GetterCase.value.getter
// FINAL-NEXT:    %[[#GETTER_FN:]] = function_ref
// FINAL-NEXT:    %[[#GETTER_PB:]] = partial_apply {{.*}} %[[#GETTER_FN]]
// FINAL-NEXT:    %[[#GETTER_RESULT:]] = tuple (%[[#]], %[[#GETTER_PB]])
// FINAL-NEXT:    return %[[#GETTER_RESULT]]
// FINAL:       } // end sil function

// FINAL-LABEL: {{^}}// reverse-mode derivative of closure #1 in makeBranchClosure()
// FINAL-NOT:     = partial_apply
// FINAL:         // function_ref specialized pullback of closure #1 in makeBranchClosure()
// FINAL-NEXT:    %[[#NESTED_FN:]] = function_ref
// FINAL-NEXT:    %[[#NESTED_PB:]] = partial_apply {{.*}} %[[#NESTED_FN]]
// FINAL-NEXT:    %[[#NESTED_RESULT:]] = tuple (%[[#]], %[[#NESTED_PB]])
// FINAL-NEXT:    return %[[#NESTED_RESULT]]
// FINAL:       } // end sil function

// FINAL-LABEL: {{^}}sil private @$s20adcs_primal_entitiesS2fcfU_TJrSpSr :
// FINAL-NOT:     = partial_apply
// FINAL:         // function_ref specialized pullback of closure #1 in
// FINAL-NEXT:    %[[#GLOBAL_FN:]] = function_ref
// FINAL-NEXT:    %[[#GLOBAL_PB:]] = partial_apply {{.*}} %[[#GLOBAL_FN]]
// FINAL-NEXT:    %[[#GLOBAL_RESULT:]] = tuple (%[[#]], %[[#GLOBAL_PB]])
// FINAL-NEXT:    return %[[#GLOBAL_RESULT]]
// FINAL:       } // end sil function

import _Differentiation

public struct GetterCase: Differentiable {
  public var x: Float

  public init(_ x: Float) { self.x = x }

  @differentiable(reverse)
  @inline(never)
  public var value: Float {
    if x > 0 { return x * x }
    return x + x
  }
}

public let branchClosure: @differentiable(reverse) (Float) -> Float = { x in
  if x > 0 { return x * x }
  return x + x
}

public func makeBranchClosure() -> @differentiable(reverse) (Float) -> Float {
  { x in
    if x > 0 { return x * x }
    return x + x
  }
}

public func getterGradient(_ x: Float) -> Float {
  gradient(at: x, of: { GetterCase($0).value })
}

public func closureGradient(_ x: Float) -> Float {
  gradient(at: x, of: branchClosure)
}
