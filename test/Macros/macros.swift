// RUN: %target-swift-frontend -enable-experimental-feature Macros -dump-ast %s -module-name MacrosTest 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx
// REQUIRES: asserts

let a = 1, b = 1
let s = #stringify(a + b)
// CHECK: macro_expansion_expr type='(Int, String)'{{.*}}name=stringify
// CHECK-NEXT: argument_list
// CHECK: tuple_expr type='(Int, String)' location=Macro expansion of #stringify

