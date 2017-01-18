// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | %FileCheck %s

// CHECK:       sil private [[INIT_A_B:@globalinit_.*]] :
// CHECK:         alloc_global @_Tv26lazy_globals_multiple_vars1aSi
// CHECK:         global_addr @_Tv26lazy_globals_multiple_vars1aSi
// CHECK:         alloc_global @_Tv26lazy_globals_multiple_vars1bSi
// CHECK:         global_addr @_Tv26lazy_globals_multiple_vars1bSi
// CHECK:       sil hidden [global_init] @_TF26lazy_globals_multiple_varsau1aSi
// CHECK:         global_addr [[TOKEN_A_B:@globalinit_.*]] :
// CHECK:         function_ref [[INIT_A_B]]
// CHECK:       sil hidden [global_init] @_TF26lazy_globals_multiple_varsau1bSi
// CHECK:         global_addr [[TOKEN_A_B]]
// CHECK:         function_ref [[INIT_A_B]]
var (a, b) = (1, 2)

// CHECK:       sil private [[INIT_C:@globalinit_.*]] :
// CHECK-NOT:     global_addr @_Tv26lazy_globals_multiple_vars1dSi
// CHECK:         alloc_global @_Tv26lazy_globals_multiple_vars1cSi
// CHECK:         global_addr @_Tv26lazy_globals_multiple_vars1cSi
// CHECK-NOT:     global_addr @_Tv26lazy_globals_multiple_vars1dSi
// CHECK:       sil hidden [global_init] @_TF26lazy_globals_multiple_varsau1cSi
// CHECK:         global_addr [[TOKEN_C:@globalinit_.*]] :
// CHECK:         function_ref [[INIT_C]]
// CHECK:       sil private [[INIT_D:@globalinit_.*]] :
// CHECK-NOT:     global_addr @_Tv26lazy_globals_multiple_vars1cSi
// CHECK:         alloc_global @_Tv26lazy_globals_multiple_vars1dSi
// CHECK:         global_addr @_Tv26lazy_globals_multiple_vars1dSi
// CHECK-NOT:     global_addr @_Tv26lazy_globals_multiple_vars1cSi
// CHECK:       sil hidden [global_init] @_TF26lazy_globals_multiple_varsau1dSi
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         global_addr [[TOKEN_D:@globalinit_.*]] :
// CHECK-NOT:     global_addr [[TOKEN_C]]
// CHECK:         function_ref [[INIT_D]]
var c = 1, d = 2
