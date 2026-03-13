// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -parse-as-library %S/Inputs/NonInlined.swift -emit-module -o %t/NonInlined.ll
// RUN: %target-swift-frontend -emit-ir -parse-as-library -g -module-name main %s -O -I %t -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s -check-prefix=TRANSPARENT-CHECK < %t.ll

// CHECK: define{{.*}}$s4main5entrys5Int64VyF
// CHECK-NOT: ret i64
// CHECK: call {{.*}}8noinline{{.*}} !dbg ![[CALL:.*]]
// CHECK: ret i64
// CHECK-DAG: ![[TOPLEVEL:.*]] = !DIFile(filename: "{{.*}}inlinescopes.swift"

import NonInlined

@_transparent
func transparent(_ x: Int64) -> Int64 { return noinline(x) }

@inline(__always)
func inlined(_ x: Int64) -> Int64 {
  let result = transparent(x)
// CHECK-DAG: ![[CALL]] = !DILocation(line: [[@LINE-1]], column: {{.*}}, scope: ![[INLINED1:.*]], inlinedAt: ![[INLINEDAT:.*]])
// CHECK-DAG: ![[INLINEDAT]] = distinct !DILocation({{.*}}scope: ![[INLINEDAT1:[0-9]+]]
// CHECK-DAG: ![[INLINED1]] = distinct !DILexicalBlock(scope: ![[INLINED:[0-9]+]]
// Check if the inlined and removed function still has the correct linkage name.
// CHECK-DAG: ![[INLINED]] = distinct !DISubprogram(name: "inlined", linkageName: "$s4main7inlinedys5Int64VADF"
// TRANSPARENT-CHECK-NOT: !DISubprogram(name: "transparent"
  return result
}
public func entry() -> Int64 { return  inlined(x) }
