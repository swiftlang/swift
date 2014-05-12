// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

// This is more of a crash test than anything else.

struct A {
  var fn : () -> ()
}

func test(var x : A) {}
// CHECK:    define void @_TF7structs4test
// CHECK:      [[X:%.*]] = alloca [[A:%.*]], align 8
// CHECK-NEXT: call void @llvm.dbg.declare(metadata !{[[A]]* [[X]]}, metadata [[X_DBG:!.*]])
// CHECK: null, null, metadata ![[A_DI:.*]]} ; [ DW_TAG_structure_type ] [A] [line [[@LINE-8]]
// CHECK: [[X_DBG]] = metadata !{i32 {{.*}}, metadata {{.*}}, metadata !"x", metadata {{.*}}, i32 {{.*}}, metadata ![[A_DI]]
