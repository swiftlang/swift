// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

func outer(a: Int) -> Int {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: !MDSubprogram(name: "inner", linkageName: "_TFF16nested_functions5outerFSiSiL_5innerfSiSi"
     // CHECK-SAME:          line: [[@LINE+1]]
     func inner(b: Int) -> Int {
       return a+b
     }

     return inner(42)
}
