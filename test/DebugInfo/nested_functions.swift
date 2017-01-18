// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// CHECK: ![[OUTER:.*]] = distinct !DISubprogram(name: "outer",
// CHECK-SAME:                                   line: [[@LINE+1]]
func outer(_ a: Int64) -> Int64 {
     // Inner functions have a linkage name of "closure[0-9]+", but
     // their DW_AT_name is preserved.

     // CHECK: !DISubprogram(name: "inner",
     // CHECK-SAME:          scope: ![[OUTER]]
     // CHECK-SAME:          line: [[@LINE+1]]
     func inner(_ b: Int64) -> Int64 {
       return a+b
     }

     return inner(42)
}
