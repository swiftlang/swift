// RUN: %target-swift-frontend -O -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK-LABEL: define{{.*}}2fn
func fn() {
  println("two")
  println(0 - UInt(Process.arguments.count))
  println(1 - UInt(Process.arguments.count))
  println("three")
}

// CHECK: ret
// CHECK-NOT: define
// CHECK: call void @llvm.trap(), !dbg ![[LOC:.*]]
// CHECK-NEXT: unreachable, !dbg ![[LOC]]
// CHECK: call void @llvm.trap(), !dbg ![[LOC2:.*]]
// CHECK-NEXT: unreachable, !dbg ![[LOC2]]
// CHECK: ![[LOC]] = !MDLocation(line: 6, column
// CHECK: ![[LOC2]] = !MDLocation(line: 7, column
