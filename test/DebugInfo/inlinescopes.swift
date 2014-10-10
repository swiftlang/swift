// RUN: %swift -target x86_64-apple-macosx10.9 %s -O -sil-inline-threshold 100 -emit-ir -g -o %t.ll
// RUN: cat %t.ll | FileCheck %s
// RUN: cat %t.ll | FileCheck %s -check-prefix=TRANSPARENT-CHECK

// CHECK: define i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[MULSCOPE:.*]]
// CHECK-DAG: ![[TOPLEVEL:.*]] = {{.*}}; [ DW_TAG_file_type ] [{{.*}}/inlinescopes.swift]
// CHECK-DAG: ![[MAIN:.*]] = {{.*}}\00main\00{{.*}}[ DW_TAG_subprogram ]
// CHECK-DAG: ![[INLINED_TOPLEVEL:.*]] = metadata !{i32 0, i32 0, metadata ![[MAIN:.*]], null}

func square(x : Int) -> Int {
// CHECK-DAG: ![[MULSCOPE]] = metadata !{i32 [[@LINE+2]], i32 {{.*}}, metadata ![[MUL:.*]], metadata ![[INLINED:.*]]}
// CHECK-DAG: ![[MUL:.*]] = {{.*}}[ DW_TAG_lexical_block ]
  let res = x * x
// *(Int, Int) is a transparent function and should not show up in the debug info.
// TRANSPARENT-CHECK-NOT: [ DW_TAG_subprogram ] {{.*}}[_TFSsoi1mFTSiSi_Si]
  return res
}
let c = Int(C_ARGC)
// CHECK-DAG ![[INLINED]] = metadata !{i32 [[@LINE+1]], i32 {{.*}}, metadata !{{.*}}, metadata ![[INLINED_TOPLEVEL:.*]]}
// CHECK-DAG: metadata ![[TOPLEVEL]]{{.*}} ; [ DW_TAG_variable ] [y] [line [[@LINE+1]]]
let y = square(c)
println(y)

