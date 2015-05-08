// RUN: %target-swift-frontend -O -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK-LABEL: define{{.*}}2fn
func fn() {
  print("two")
// CHECK-DAG: ![[LOC:.*]] = !DILocation(line: [[@LINE+1]], column: 11,
  print(0 - UInt(Process.arguments.count))
// CHECK-DAG: ![[LOC2:.*]] = !DILocation(line: [[@LINE+1]], column: 11,
  print(1 - UInt(Process.arguments.count))
  print("three")
}

// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC]]
// CHECK-DAG: unreachable, !dbg ![[LOC]]
// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC2]]
// CHECK-DAG: unreachable, !dbg ![[LOC2]]
