// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// https://github.com/apple/swift/issues/73030

// CHECK-LABEL: sil {{.+}} @$s4main1f1t1cxxQp_q_t_q0_txxQp_t_q_t_q0_tRvzr1_lF
func f<each A, B, C>(t: ((repeat each A), B), c: C) -> ((repeat each A, B), C) {
  // CHECK-NOT: let, name "t"
  // CHECK: alloc_stack [lexical] [var_decl] $((repeat each A), B), let, name "t", argno 1
  // CHECK: debug_value %{{[0-9]+}} : $*C, let, name "c", argno 2, expr op_deref
  ((repeat each t.0, t.1), c)
}
