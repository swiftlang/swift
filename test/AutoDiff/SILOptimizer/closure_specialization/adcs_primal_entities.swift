// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_primal_entities %S/../../validation-test/closure_specialization/adcs_primal_entities.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=GETTER < %t.passes
// RUN: %FileCheck %s --check-prefix=NESTED < %t.passes
// RUN: %FileCheck %s --check-prefix=GLOBAL < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// Compile the runtime regression's Swift definitions, preserving real
// compiler-generated tapes. Check that ADCS creates specialized pullbacks
// for each kind of primal entity and directly invokes the saved callback.

// Each input stores a captured multiplication pullback in a branch tape.
// INPUT-LABEL: // reverse-mode derivative of GetterCase.value.getter
// INPUT: %[[GETTER_CB:[0-9]+]] = partial_apply
// INPUT-NEXT: %[[GETTER_PAYLOAD:[0-9]+]] = tuple {{.*}}%[[GETTER_CB]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[GETTER_PAYLOAD]]
// INPUT-LABEL: // reverse-mode derivative of closure #1 in makeBranchClosure()
// INPUT: %[[NESTED_CB:[0-9]+]] = partial_apply
// INPUT-NEXT: %[[NESTED_PAYLOAD:[0-9]+]] = tuple {{.*}}%[[NESTED_CB]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[NESTED_PAYLOAD]]
// INPUT-LABEL: sil private [ossa] @$s20adcs_primal_entitiesS2fcfU_TJrSpSr :
// INPUT: %[[GLOBAL_CB:[0-9]+]] = partial_apply
// INPUT-NEXT: %[[GLOBAL_PAYLOAD:[0-9]+]] = tuple {{.*}}%[[GLOBAL_CB]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[GLOBAL_PAYLOAD]]

// GETTER-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}GetterCaseV5valueSfvgTJp
// GETTER-NEXT: // specialized pullback of GetterCase.value.getter
// GETTER: // specialized pullback of GetterCase.value.getter
// GETTER-NOT: = partial_apply
// GETTER-NOT: apply {{.*}} : $@callee_guaranteed
// GETTER: %[[MULTIPLY:[0-9]+]] = function_ref @{{.*}}_vjpMultiply
// GETTER-NEXT: {{.*}} = apply %[[MULTIPLY]]({{.*}}) : $@convention(thin)
// GETTER-NOT: = partial_apply
// GETTER-NOT: apply {{.*}} : $@callee_guaranteed
// GETTER: } // end sil function

// The enclosing named function is not the closure's primal entity.
// NESTED-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}makeBranchClosure{{.*}}cfU_TJp
// NESTED-NEXT: // specialized pullback of closure #1 in makeBranchClosure()
// NESTED: // specialized pullback of closure #1 in makeBranchClosure()
// NESTED-NOT: = partial_apply
// NESTED-NOT: apply {{.*}} : $@callee_guaranteed
// NESTED: %[[MULTIPLY:[0-9]+]] = function_ref @{{.*}}_vjpMultiply
// NESTED-NEXT: {{.*}} = apply %[[MULTIPLY]]({{.*}}) : $@convention(thin)
// NESTED-NOT: = partial_apply
// NESTED-NOT: apply {{.*}} : $@callee_guaranteed
// NESTED: } // end sil function

// A global initializer's closure has no enclosing named function at all.
// GLOBAL-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: $s20adcs_primal_entitiesS2fcfU_TJp
// GLOBAL-NEXT: // specialized pullback of closure #1 in
// GLOBAL: // specialized pullback of closure #1 in
// GLOBAL-NOT: = partial_apply
// GLOBAL-NOT: apply {{.*}} : $@callee_guaranteed
// GLOBAL: %[[MULTIPLY:[0-9]+]] = function_ref @{{.*}}_vjpMultiply
// GLOBAL-NEXT: {{.*}} = apply %[[MULTIPLY]]({{.*}}) : $@convention(thin)
// GLOBAL-NOT: = partial_apply
// GLOBAL-NOT: apply {{.*}} : $@callee_guaranteed
// GLOBAL: } // end sil function

// The final VJPs capture data in specialized tapes, retaining only the
// partial application of the top-level specialized pullback.
// FINAL-LABEL: // reverse-mode derivative of GetterCase.value.getter
// FINAL-NOT: = partial_apply
// FINAL: // function_ref specialized pullback of GetterCase.value.getter
// FINAL-NEXT: %[[GETTER_FN:[0-9]+]] = function_ref
// FINAL-NEXT: %[[GETTER_PB:[0-9]+]] = partial_apply {{.*}} %[[GETTER_FN]]
// FINAL-NEXT: %[[GETTER_RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[GETTER_PB]])
// FINAL-NEXT: return %[[GETTER_RESULT]]
// FINAL: } // end sil function

// FINAL-LABEL: // reverse-mode derivative of closure #1 in makeBranchClosure()
// FINAL-NOT: = partial_apply
// FINAL: // function_ref specialized pullback of closure #1 in makeBranchClosure()
// FINAL-NEXT: %[[NESTED_FN:[0-9]+]] = function_ref
// FINAL-NEXT: %[[NESTED_PB:[0-9]+]] = partial_apply {{.*}} %[[NESTED_FN]]
// FINAL-NEXT: %[[NESTED_RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[NESTED_PB]])
// FINAL-NEXT: return %[[NESTED_RESULT]]
// FINAL: } // end sil function

// FINAL-LABEL: sil private @$s20adcs_primal_entitiesS2fcfU_TJrSpSr :
// FINAL-NOT: = partial_apply
// FINAL: // function_ref specialized pullback of closure #1 in
// FINAL-NEXT: %[[GLOBAL_FN:[0-9]+]] = function_ref
// FINAL-NEXT: %[[GLOBAL_PB:[0-9]+]] = partial_apply {{.*}} %[[GLOBAL_FN]]
// FINAL-NEXT: %[[GLOBAL_RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[GLOBAL_PB]])
// FINAL-NEXT: return %[[GLOBAL_RESULT]]
// FINAL: } // end sil function
