// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "public var x = Int64()" | %target-swift-frontend -module-name FooBar -emit-module -o %t -
// RUN: %target-swift-frontend %s -O -I %t -sil-inline-threshold 100 -emit-ir -g -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: FileCheck %s -check-prefix=TRANSPARENT-CHECK < %t.ll

// CHECK: define i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[MULSCOPE:.*]]
// CHECK-DAG: ![[TOPLEVEL:.*]] = !MDFile(filename: "inlinescopes.swift"
// CHECK-DAG: ![[MAIN:.*]] = !MDSubprogram(name: "main"
// CHECK-DAG: ![[INLINED_TOPLEVEL:.*]] = !MDLocation(line: 0, scope: ![[MAIN:.*]])

import FooBar

func square(x: Int64) -> Int64 {
// CHECK-DAG: ![[MULSCOPE]] = !MDLocation(line: [[@LINE+2]], column: {{.*}}, scope: ![[MUL:.*]], inlinedAt: ![[INLINED:.*]])
// CHECK-DAG: ![[MUL:.*]] = distinct !MDLexicalBlock(
  let res = x * x
// *(Int, Int) is a transparent function and should not show up in the debug info.
// TRANSPARENT-CHECK-NOT: !MDSubprogram(name: "_TFSsoi1mFTSiSi_Si"
  return res
}
let c = Int64(x)
// CHECK-DAG ![[INLINED]] = !MDLocation(i32 [[@LINE+1]], column: {{.*}}, scope: !{{.*}}, inlinedAt: ![[INLINED_TOPLEVEL:.*]])
// CHECK-DAG: !MDGlobalVariable(name: "y",{{.*}} file: ![[TOPLEVEL]],{{.*}} line: [[@LINE+1]]
let y = square(c)
println(y)

// Check if the inlined and removed square function still has the correct linkage name in the debug info.
// CHECK-DAG: !MDSubprogram(name: "square", linkageName: "_TF4main6squareFVSs5Int64S0_"
