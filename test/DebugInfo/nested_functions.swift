// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

func outer(a: Int) -> Int {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: \00_TFF16nested_functions5outerFSiSiL_5innerfSiSi\00[[@LINE+1]]\000\001\000\000\000\00{{.*}}[ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] {{.*}}[inner]
     func inner(b: Int) -> Int {
       return a+b
     }

     return inner(42)
}
