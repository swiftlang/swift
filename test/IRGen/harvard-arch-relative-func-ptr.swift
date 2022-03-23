// RUN: %swift -target wasm32-unknown-wasi -parse-stdlib -emit-ir -o - %s | %FileCheck %s

// REQUIRES: CODEGENERATOR=WebAssembly

// Ensure that relative function pointer in entry_point is indirect on harvard archs
//
// CHECK:      @indirect.main = private unnamed_addr constant i32 (i32, i8*)* @main
// CHECK:      @"\01l_entry_point" = private constant { i32 } {
// CHECK-SAME:   i32 sub (
// CHECK-SAME:     i32 ptrtoint (i32 (i32, i8*)** @indirect.main to i32),
// CHECK-SAME:     i32 ptrtoint ({ i32 }* @"\01l_entry_point" to i32)
// CHECK-SAME:   )
// CHECK-SAME: }, section "swift5_entry", align 4
