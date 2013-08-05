// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

func outer(a: Int) -> Int {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: i1 false, i1 true, i32 0, i32 0, null, i32 0,{{.*}}[ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] {{.*}}[inner]
     func inner(b: Int) -> Int {
       return a+b
     }

     return inner(42)
}
