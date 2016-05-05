// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "public var x = Int64()" | %target-swift-frontend -module-name FooBar -emit-module -o %t -
// RUN: %target-swift-frontend %s -O -I %t -emit-ir -g -o %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: FileCheck %s -check-prefix=TRANSPARENT-CHECK < %t.ll

// CHECK: define{{( protected)?( signext)?}} i32 @main
// CHECK: tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %[[C:.*]], i64 %[[C]]), !dbg ![[MULSCOPE:.*]]
// CHECK-DAG: ![[TOPLEVEL:.*]] = !DIFile(filename: "inlinescopes.swift"

import FooBar

func markUsed<T>(_ t: T) {}

@inline(__always)
func square(_ x: Int64) -> Int64 {
// CHECK-DAG: ![[MULSCOPE]] = !DILocation(line: [[@LINE+2]], column: {{.*}}, scope: ![[MUL:.*]], inlinedAt: ![[INLINED:.*]])
// CHECK-DAG: ![[MUL:.*]] = distinct !DILexicalBlock(
  let res = x * x
// *(Int, Int) is a transparent function and should not show up in the debug info.
// TRANSPARENT-CHECK-NOT: !DISubprogram(name: "_TFsoi1mFTSiSi_Si"
  return res
}
let c = Int64(x)
// CHECK-DAG: !DIGlobalVariable(name: "y",{{.*}} file: ![[TOPLEVEL]],{{.*}} line: [[@LINE+1]]
let y = square(c)
markUsed(y)

// Check if the inlined and removed square function still has the correct linkage name in the debug info.
// CHECK-DAG: !DISubprogram(name: "square", linkageName: "_TF4main6squareFVs5Int64S0_"
