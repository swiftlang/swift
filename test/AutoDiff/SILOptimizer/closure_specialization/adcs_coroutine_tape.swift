// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_coroutine_tape %S/../../validation-test/closure_specialization/adcs_coroutine_tape.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT --implicit-check-not='specialized pullback of mutateAndBranch' < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// The VJP saves both a converted accessor pullback and a known multiplication
// pullback in branch-tracing enum payloads.
// INPUT-LABEL: // reverse-mode derivative of mutateAndBranch(_:_:)
// INPUT: %[[CONVERTED:[0-9]+]] = convert_function %{{[0-9]+}} to $@callee_guaranteed (@inout Accumulator.TangentVector) -> ()
// INPUT: %[[SAVED:[0-9]+]] = tuple (%{{[0-9]+}}, %[[CONVERTED]], %{{[0-9]+}})
// INPUT: {{.*}} = enum $_AD__{{.*}}, %[[SAVED]]
// INPUT: %[[STATIC:[0-9]+]] = function_ref @{{.*}}_vjpMultiply
// INPUT-NEXT: %[[CLOSURE:[0-9]+]] = partial_apply {{.*}} %[[STATIC]]
// INPUT: %[[KNOWN:[0-9]+]] = tuple $(predecessor: {{.*}}) ({{.*}}, %[[CLOSURE]])
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[KNOWN]]

// The saved accessor field must be converted back and invoked as a coroutine.
// That use is outside ADCS's direct-apply rewrite contract.
// INPUT-LABEL: // pullback of mutateAndBranch(_:_:)
// INPUT: bb{{[0-9]+}}({{.*}}, %[[PAYLOAD:[0-9]+]] : @owned $({{.*}}, @callee_guaranteed (@inout Accumulator.TangentVector) -> (), {{.*}})):
// INPUT-NEXT: (%{{[0-9]+}}, %[[TAPED:[0-9]+]], %{{[0-9]+}}) = destructure_tuple %[[PAYLOAD]]
// INPUT-NEXT: %[[ACCESSOR:[0-9]+]] = convert_function %[[TAPED]] to $@yield_once
// INPUT-NEXT: (%{{[0-9]+}}, %[[TOKEN:[0-9]+]]) = begin_apply %[[ACCESSOR]]
// INPUT: end_apply %[[TOKEN]]

// FINAL-LABEL: // reverse-mode derivative of mutateAndBranch(_:_:)
// FINAL-NOT: _spec
// FINAL: // function_ref pullback of mutateAndBranch(_:_:)
// FINAL-NOT: _spec
// FINAL: } // end sil function
// FINAL-LABEL: // pullback of mutateAndBranch(_:_:)
// FINAL: ({{.*}}, %[[FINAL_TOKEN:[0-9]+]]) = begin_apply {{.*}} : $@yield_once
// FINAL: end_apply %[[FINAL_TOKEN]]
// FINAL: } // end sil function
