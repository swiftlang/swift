// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "public var x = Int64()" | %target-swift-frontend -module-name FooBar -emit-module -o %t -
// RUN: %target-swift-frontend %s -O -I %t -sil-inline-threshold 100 -emit-ir -g -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: FileCheck %s -check-prefix=TRANSPARENT-CHECK < %t.ll

// CHECK: define i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[MULSCOPE:.*]]
// CHECK-DAG: ![[TOPLEVEL:.*]] = {{.*}}; [ DW_TAG_file_type ] [{{.*}}/inlinescopes.swift]
// CHECK-DAG: ![[MAIN:.*]] = {{.*}}\00main\00{{.*}}[ DW_TAG_subprogram ]
// CHECK-DAG: ![[INLINED_TOPLEVEL:.*]] = !MDLocation(line: 0, scope: ![[MAIN:.*]])

import FooBar

func square(x: Int64) -> Int64 {
// CHECK-DAG: ![[MULSCOPE]] = !MDLocation(line: [[@LINE+2]], column: {{.*}}, scope: ![[MUL:.*]], inlinedAt: ![[INLINED:.*]])
// CHECK-DAG: ![[MUL:.*]] = {{.*}}[ DW_TAG_lexical_block ]
  let res = x * x
// *(Int, Int) is a transparent function and should not show up in the debug info.
// TRANSPARENT-CHECK-NOT: [ DW_TAG_subprogram ] {{.*}}[_TFSsoi1mFTSiSi_Si]
  return res
}
let c = Int64(x)
// CHECK-DAG ![[INLINED]] = !MDLocation(i32 [[@LINE+1]], column: {{.*}}, scope: !{{.*}}, inlinedAt: ![[INLINED_TOPLEVEL:.*]])
// CHECK-DAG: ![[TOPLEVEL]]{{.*}} ; [ DW_TAG_variable ] [y] [line [[@LINE+1]]]
let y = square(c)
println(y)

// Check if the inlined and removed square function still has the correct linkage name in the debug info.
// CHECK-DAG: _TF4main6squareFVSs5Int64S0_{{.*}}; [ DW_TAG_subprogram ] {{.*}}[square]
