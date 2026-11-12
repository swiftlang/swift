// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_never_callback %S/../../validation-test/closure_specialization/adcs_never_callback.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.passes
// RUN: %FileCheck %s --check-prefix=AFTER < %t.passes

// The root has an indirect Never error result, but the callback actually
// stored in the tape is its nonthrowing reabstraction. Keep specializing that
// callback rather than rejecting every throwing root in an adapter chain.
// BEFORE-LABEL: // reverse-mode derivative of branchGenericNever(_:_:)
// BEFORE: // function_ref specialized closure #1 in genericPullback<A>(_:_:)
// BEFORE-NEXT: %[[ROOT_FN:[0-9]+]] = function_ref
// BEFORE-NEXT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} %[[ROOT_FN]](%{{[0-9]+}}) : {{.*}} (Float, Float) -> (Float, @error_indirect Never)
// BEFORE: // function_ref thunk for @escaping
// BEFORE-NEXT: %[[THUNK_FN:[0-9]+]] = function_ref {{.*}} : {{.*}} (Float, @guaranteed @callee_guaranteed (Float) -> (Float, @error_indirect Never)) -> Float
// BEFORE-NEXT: %[[CALLBACK:[0-9]+]] = partial_apply {{.*}} %[[THUNK_FN]](%[[ROOT]])
// BEFORE-NEXT: %[[PAYLOAD:[0-9]+]] = tuple {{.*}}@callee_guaranteed (Float) -> Float) ({{.*}}%[[CALLBACK]])
// BEFORE-NEXT: {{.*}} = enum {{.*}}, %[[PAYLOAD]]

// ADCS replaces the adapted callback with its Float capture. It recreates the
// throwing root and directly invokes the nonthrowing adapter, whose internal
// apply [nothrow] remains in the original thunk body.
// AFTER-LABEL: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}18branchGenericNever{{.*}}TJp
// AFTER-NEXT: // specialized pullback of branchGenericNever(_:_:)
// AFTER: // specialized pullback of branchGenericNever(_:_:)
// AFTER: bb{{[0-9]+}}(%[[PAYLOAD:[0-9]+]] : $(predecessor: {{.*}}, (_: Float))):
// AFTER-NEXT: (%{{[0-9]+}}, %[[CAPTURES:[0-9]+]]) = destructure_tuple %[[PAYLOAD]]
// AFTER-NEXT: %[[CAPTURE:[0-9]+]] = tuple_extract %[[CAPTURES]], 0
// AFTER: // function_ref specialized closure #1 in genericPullback<A>(_:_:)
// AFTER-NEXT: %[[ROOT_FN:[0-9]+]] = function_ref
// AFTER-NEXT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} %[[ROOT_FN]](%[[CAPTURE]]) : {{.*}} (Float, Float) -> (Float, @error_indirect Never)
// AFTER: // function_ref thunk for @escaping
// AFTER-NEXT: %[[THUNK_FN:[0-9]+]] = function_ref {{.*}} : {{.*}} (Float, @guaranteed @callee_guaranteed (Float) -> (Float, @error_indirect Never)) -> Float
// AFTER-NEXT: {{.*}} = apply %[[THUNK_FN]](%{{[0-9]+}}, %[[ROOT]]) : {{.*}} -> Float
// AFTER: destroy_value %[[ROOT]]
// AFTER: } // end sil function
