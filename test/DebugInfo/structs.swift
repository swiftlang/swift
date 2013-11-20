// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// This is more of a crash test than anything else.

struct A {
  var fn : () -> ()
}

func test(x : A) {}
// CHECK:    define void @_T7structs4testFT1xVS_1A_T_
// CHECK:      [[X:%.*]] = alloca [[A:%.*]], align 8
// CHECK-NEXT: call void @llvm.dbg.declare(metadata !{[[A]]* [[X]]}, metadata [[X_DBG:!.*]])

// CHECK: [[A_DI:!.*]] = metadata !{i32 786451, metadata {{.*}}, metadata {{.*}}, metadata !"_TtV7structs1A",
// CHECK: [[X_DBG]] = metadata !{i32 {{.*}}, metadata {{.*}}, metadata !"x", metadata {{.*}}, i32 {{.*}}, metadata [[A_DI]]
