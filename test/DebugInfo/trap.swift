// RUN: %target-swift-frontend -parse-stdlib -primary-file %s -emit-ir -g -o - | FileCheck %s

import Swift
// CHECK: define{{.*}}1f
func f(x : Int) -> Int {
  if x < 23 {
    // CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC1:.*]]
    // CHECK-DAG: ![[LOC1]] = !DILocation(line: [[@LINE+1]],
    Builtin.int_trap()
  }
  if x > 42 {
    // CHECK-DAG: ![[LOC2:.*]] = !DILocation(line: [[@LINE+1]],
    Builtin.int_trap()
    // CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC2]]
  }
  return x
}
