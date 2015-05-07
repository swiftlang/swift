// RUN: %target-swift-frontend -O -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK-LABEL: define{{.*}}2fn
func fn() {
  println("two")
// CHECK-DAG: ![[LOC:.*]] = !DILocation(line: [[@LINE+1]], column: 13,
  println(0 - UInt(Process.arguments.count))
// CHECK-DAG: ![[LOC2:.*]] = !DILocation(line: [[@LINE+1]], column: 13,
  println(1 - UInt(Process.arguments.count))
  println("three")
}

// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC]]
// CHECK-DAG: unreachable, !dbg ![[LOC]]
// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC2]]
// CHECK-DAG: unreachable, !dbg ![[LOC2]]
