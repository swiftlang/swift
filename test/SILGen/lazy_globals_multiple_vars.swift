// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | %FileCheck %s

// CHECK:       sil private [[INIT_A_B:@globalinit_.*]] :
// CHECK:         alloc_global @_T026lazy_globals_multiple_vars1aSiv
// CHECK:         global_addr @_T026lazy_globals_multiple_vars1aSiv
// CHECK:         alloc_global @_T026lazy_globals_multiple_vars1bSiv
// CHECK:         global_addr @_T026lazy_globals_multiple_vars1bSiv
// CHECK:       sil hidden [global_init] @_T026lazy_globals_multiple_vars1aSifau
// CHECK:         global_addr [[TOKEN_A_B:@globalinit_.*]] :
// CHECK:         function_ref [[INIT_A_B]]
// CHECK:       sil hidden [global_init] @_T026lazy_globals_multiple_vars1bSifau
// CHECK:         global_addr [[TOKEN_A_B]]
// CHECK:         function_ref [[INIT_A_B]]
var (a, b) = (1, 2)

// CHECK:       sil private [[INIT_C:@globalinit_.*]] :
// CHECK-NOT:     global_addr @_T026lazy_globals_multiple_vars1dSiv
// CHECK:         alloc_global @_T026lazy_globals_multiple_vars1cSiv
// CHECK:         global_addr @_T026lazy_globals_multiple_vars1cSiv
// CHECK-NOT:     global_addr @_T026lazy_globals_multiple_vars1dSiv
// CHECK:       sil hidden [global_init] @_T026lazy_globals_multiple_vars1cSifau
// CHECK:         global_addr [[TOKEN_C:@globalinit_.*]] :
// CHECK:         function_ref [[INIT_C]]
// CHECK:       sil private [[INIT_D:@globalinit_.*]] :
// CHECK-NOT:     global_addr @_T026lazy_globals_multiple_vars1cSiv
// CHECK:         alloc_global @_T026lazy_globals_multiple_vars1dSiv
// CHECK:         global_addr @_T026lazy_globals_multiple_vars1dSiv
// CHECK-NOT:     global_addr @_T026lazy_globals_multiple_vars1cSiv
// CHECK:       sil hidden [global_init] @_T026lazy_globals_multiple_vars1dSifau
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         global_addr [[TOKEN_D:@globalinit_.*]] :
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         function_ref [[INIT_D]]
var c = 1, d = 2
