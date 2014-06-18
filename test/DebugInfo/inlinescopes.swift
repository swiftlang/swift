// RUN: %swift -target x86_64-apple-darwin %s -O1 -sil-inline-threshold 100 -emit-ir -g -o - | FileCheck %s

// CHECK: define i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[MULSCOPE:.*]]
// CHECK: ![[TOPLEVEL:.*]] = {{.*}}; [ DW_TAG_file_type ] [{{.*}}/inlinescopes.swift]
// CHECK=DAG: ![[MAIN:.*]] = {{.*}}"main"{{.*}}[ DW_TAG_subprogram ]
// CHECK=DAG: metadata !{i32 786484, {{.*}}metadata !"_Tv12inlinescopes1ySi", metadata ![[TOPLEVEL]]{{.*}} ; [ DW_TAG_variable ] [y]
// CHECK-DAG: ![[INLINED_TOPLEVEL:.*]] = metadata !{i32 0, i32 0, metadata ![[MAIN:.*]], null}

func square(x : Int) -> Int {
// CHECK-DAG: ![[MULSCOPE]] = metadata !{i32 [[@LINE+3]], i32 {{.*}}, metadata ![[MULBLOCK:.*]], metadata ![[INLINED:.*]]}
// CHECK-DAG: ![[MUL:.*]] = {{.*}}[ DW_TAG_subprogram ] [line 0] [def] [_TFSsoi1mFTSiSi_Si]
// CHECK-DAG: ![[MULBLOCK]] = {{.*}} metadata ![[MUL]]} ; [ DW_TAG_lexical_block ]
  let res = x * x
  return res
}
let c = Int(C_ARGC)
// CHECK-DAG ![[INLINED]] = metadata !{i32 [[@LINE+1]], i32 {{.*}}, metadata !{{.*}}, metadata ![[INLINED_TOPLEVEL:.*]]}
let y = square(c)
println(y)

