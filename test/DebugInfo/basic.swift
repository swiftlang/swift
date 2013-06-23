// A very basic test for debug info.

// Verify that we don't emit any debug info by default.
// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -o - | FileCheck %s --check-prefix NDEBUG
// NDEBUG-NOT: !dbg
// NDEBUG-NOT: DW_TAG

// Now check that we do generate line+scope info with -g.
// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

func f(a: Int, b:Int) -> Int {
     // CHECK-DAG: i32 [[@LINE-1]],{{.*}}DW_TAG_lexical_block
     if b != 0 {
       // CHECK-DAG: i32 [[@LINE-1]],{{.*}}DW_TAG_lexical_block
       // CHECK-DAG: ret{{.*}}, !dbg ![[DBG:[0-9]+]]
       // CHECK-DAG: [[DBG]] = metadata !{i32 [[@LINE+1]],
       return a/b
     } else {
       // CHECK-DAG: [[PARENT:[0-9]+]] = {{.*}}i32 [[@LINE-1]],{{.*}}DW_TAG_lexical_block
       var c = 42
       if a == 0 {
         // CHECK-DAG: metadata ![[PARENT]], i32 [[@LINE-1]],{{.*}}DW_TAG_lexical_block
         // What about a nested scope?
         return 0
       }
       return c
     }
}
