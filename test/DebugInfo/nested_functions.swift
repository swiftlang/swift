// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

func outer(a: Int64) -> Int64 {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: !DISubprogram(name: "inner", linkageName: "_TFF16nested_functions5outerFVs5Int64S0_L_5innerfS0_S0_"
     // CHECK-SAME:          line: [[@LINE+1]]
     func inner(b: Int64) -> Int64 {
       return a+b
     }

     return inner(42)
}
