// RUN: %swift -target x86_64-apple-darwin %s -O1 -sil-inline-threshold 100 -emit-ir -g -o - | FileCheck %s

// CHECK: define i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[SCOPE:.*]]
// CHECK: ![[TOPLEVEL:.*]] = {{.*}}; [ DW_TAG_file_type ] [{{.*}}/inlinescopes.swift]
// CHECK: ![[MAIN:.*]] = {{.*}}"main"{{.*}}[ DW_TAG_subprogram ]
// CHECK: metadata !{i32 786484, {{.*}}metadata !"_Tv12inlinescopes1ySi", metadata ![[TOPLEVEL]]{{.*}} ; [ DW_TAG_variable ] [y]
// CHECK: ![[INLINED_TOPLEVEL:.*]] = metadata !{i32 0, i32 0, metadata ![[MAIN:.*]], null}

func square(x : Int) -> Int {
// CHECK: ![[SCOPE]] = metadata !{i32 [[@LINE+2]], i32 {{.*}}, metadata ![[PARENT:.*]], metadata ![[INLINED:.*]]}
// CHECK: ![[PARENT]] = metadata !{i32 786443,
  let res = x * x
  return res
}
let c = Int(C_ARGC)
// CHECK: ![[INLINED]] = metadata !{i32 [[@LINE+1]], i32 {{.*}}, metadata !{{.*}}, metadata ![[INLINED_TOPLEVEL:.*]]}
let y = square(c)
println(y)

