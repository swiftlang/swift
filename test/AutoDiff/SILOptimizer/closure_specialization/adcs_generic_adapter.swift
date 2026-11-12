// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_generic_adapter %S/../../validation-test/closure_specialization/adcs_generic_adapter.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.passes
// RUN: %FileCheck %s --check-prefix=THIN < %t.passes
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_generic_adapter.swift < %t.sil

// Recreate the root and its reabstraction thunk using the cloned pullback's
// generic parameter. Call the outer subset thunk directly.
// CHECK: sil {{.*}}[reabstraction_thunk] {{.*}} @[[REABSTRACTION:[^ ]+]] : {{.*}} (Float, @guaranteed @callee_guaranteed (@in_guaranteed Float) ->
// CHECK-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{[^ ]*}}6Subset{{[^ ]*}}TJp
// CHECK-NEXT: // specialized pullback of genericSubset<A>(_:_:_:)
// CHECK: // specialized pullback of genericSubset<A>(_:_:_:)
// CHECK-NOT: } // end sil function
// CHECK: // function_ref closure #1 in makePullback<A, B>(_:_:)
// CHECK-NEXT: %[[ROOT:[0-9]+]] = function_ref
// CHECK-NEXT: %[[INNER:[0-9]+]] = partial_apply {{.*}} %[[ROOT]]<Float, τ_0_0>()
// CHECK-NEXT: // function_ref thunk for @escaping
// CHECK-NEXT: %[[THUNK:[0-9]+]] = function_ref @[[REABSTRACTION]]
// CHECK-NEXT: %[[ADAPTED:[0-9]+]] = partial_apply {{.*}} %[[THUNK]]<τ_0_0>(%[[INNER]])
// CHECK-NEXT: // function_ref autodiff subset parameters thunk for pullback
// CHECK-NEXT: %[[SUBSET:[0-9]+]] = function_ref
// CHECK-NEXT: {{.*}} = apply %[[SUBSET]]<τ_0_0>(%{{[0-9]+}}, %[[ADAPTED]])
// CHECK: destroy_value %[[ADAPTED]]
// CHECK: } // end sil function

// A captureless nongeneric root is recreated with thin_to_thick.
// THIN-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{[^ ]*}}10thinSubset{{[^ ]*}}TJp
// THIN-NEXT: // specialized pullback of thinSubset(_:_:_:)
// THIN: // specialized pullback of thinSubset(_:_:_:)
// THIN-NOT: } // end sil function
// THIN: %[[ADD:[0-9]+]] = function_ref @{{.*}}_vjpAdd
// THIN-NEXT: %[[THICK:[0-9]+]] = thin_to_thick_function %[[ADD]]
// THIN-NEXT: // function_ref autodiff subset parameters thunk for pullback
// THIN-NEXT: %[[SUBSET:[0-9]+]] = function_ref
// THIN-NEXT: {{.*}} = apply %[[SUBSET]](%{{[0-9]+}}, %[[THICK]])
