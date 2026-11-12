// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name NestedTape %S/../../validation-test/closure_specialization/adcs_nested_tape.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s --check-prefix=BOUND < %t.log

// The String VJP contains a separate Int tape captured by a nested pullback.
// Find the original VJP, then its rewritten body at the first ADCS invocation.
// BOUND-LABEL: sil shared {{.*}}@$s10NestedTape06nestedB0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5
// BOUND: } // end sil function
// BOUND-LABEL: sil shared {{.*}}@$s10NestedTape06nestedB0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5

// The outer tape is specialized, eliminating the nested pullback's allocated
// closure context. Only the returned outer pullback still needs partial_apply.
// BOUND: bb1(
// BOUND: [[OUTER_REF:%[0-9]+]] = function_ref @[[OUTER_PB:[^ ]+]] : $@convention(thin) (Float, @owned {{_AD__[^ )]+_spec[^ )]+}}) -> Float
// BOUND-NEXT: [[RETURNED_PB:%[0-9]+]] = partial_apply [callee_guaranteed] [[OUTER_REF]]
// BOUND-NEXT: {{%[0-9]+}} = tuple ({{%[0-9]+}}, [[RETURNED_PB]])
// BOUND-NEXT: return

// Preserve the nested tape's function field and enum type. Its callback is
// captureless, so this does not allocate a closure context.
// BOUND: bb2:
// BOUND-NOT: = partial_apply
// BOUND: [[ADD:%[0-9]+]] = thin_to_thick_function {{.*}} to $@callee_guaranteed (Float) -> (Float, Float)
// BOUND-NEXT: [[INNER_PAYLOAD:%[0-9]+]] = tuple $(predecessor: {{_AD__[^, ]+}}<Int>, @callee_guaranteed (Float) -> (Float, Float)) ({{.*}}[[ADD]])
// BOUND-NEXT: [[INNER_TAPE:%[0-9]+]] = enum $[[INNER_TYPE:_AD__[^, ]+<Int>]], {{.*}}[[INNER_PAYLOAD]]
// BOUND-NOT: = partial_apply
// BOUND: function_ref @[[INNER_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[INNER_TYPE]]) -> Float
// BOUND-NOT: = partial_apply
// BOUND: tuple ([[INNER_TAPE]])
// BOUND-NOT: = partial_apply
// BOUND: bb3:
// BOUND-NOT: = partial_apply
// BOUND: } // end sil function

// The specialized outer pullback invokes the known inner pullback directly,
// passing the preserved Int tape instead of reconstructing its closure.
// BOUND: sil private [ossa] @[[OUTER_PB]]
// BOUND-NOT: = partial_apply
// BOUND: [[DIRECT:%[0-9]+]] = function_ref @[[INNER_PB]]
// BOUND-NEXT: {{%[0-9]+}} = apply [[DIRECT]]
