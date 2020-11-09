// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

// CHECK:       sil private [global_init_once_fn] [ossa] [[INIT_A_B:@.*1a.*1b.*WZ]] :
// CHECK:         alloc_global @$s26lazy_globals_multiple_vars1aSiv
// CHECK:         global_addr @$s26lazy_globals_multiple_vars1aSiv
// CHECK:         alloc_global @$s26lazy_globals_multiple_vars1bSiv
// CHECK:         global_addr @$s26lazy_globals_multiple_vars1bSiv
// CHECK:       sil hidden [global_init] [ossa] @$s26lazy_globals_multiple_vars1aSivau
// CHECK:         global_addr [[TOKEN_A_B:@.*1a.*1b.*Wz]] :
// CHECK:         function_ref [[INIT_A_B]]
// CHECK:       sil hidden [global_init] [ossa] @$s26lazy_globals_multiple_vars1bSivau
// CHECK:         global_addr [[TOKEN_A_B]]
// CHECK:         function_ref [[INIT_A_B]]
var (a, b) = (1, 2)

// CHECK:       sil private [global_init_once_fn] [ossa] [[INIT_C:@.*1c.*WZ]] :
// CHECK-NOT:     global_addr @$s26lazy_globals_multiple_vars1dSiv
// CHECK:         alloc_global @$s26lazy_globals_multiple_vars1cSiv
// CHECK:         global_addr @$s26lazy_globals_multiple_vars1cSiv
// CHECK-NOT:     global_addr @$s26lazy_globals_multiple_vars1dSiv
// CHECK:       sil hidden [global_init] [ossa] @$s26lazy_globals_multiple_vars1cSivau
// CHECK:         global_addr [[TOKEN_C:@.*1c.*Wz]] :
// CHECK:         function_ref [[INIT_C]]
// CHECK:       sil private [global_init_once_fn] [ossa] [[INIT_D:@.*1d.*WZ]] :
// CHECK-NOT:     global_addr @$s26lazy_globals_multiple_vars1cSiv
// CHECK:         alloc_global @$s26lazy_globals_multiple_vars1dSiv
// CHECK:         global_addr @$s26lazy_globals_multiple_vars1dSiv
// CHECK-NOT:     global_addr @$s26lazy_globals_multiple_vars1cSiv
// CHECK:       sil hidden [global_init] [ossa] @$s26lazy_globals_multiple_vars1dSivau
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         global_addr [[TOKEN_D:@.*1d.*Wz]] :
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         function_ref [[INIT_D]]
var c = 1, d = 2
