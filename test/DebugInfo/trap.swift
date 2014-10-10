// RUN: %swift -parse-stdlib -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir -g -o - | FileCheck %s

import Swift
func f(x : Int) -> Int {
  if x < 23 {
    // CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC1:.*]]
    // CHECK-DAG: ![[LOC1]] = metadata !{i32 [[@LINE+1]],
    Builtin.int_trap()
  }
  if x > 42 {
    // CHECK-DAG: ![[LOC2:.*]] = metadata !{i32 [[@LINE+1]],
    Builtin.int_trap()
    // CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC2]]
  }
  return x
}
