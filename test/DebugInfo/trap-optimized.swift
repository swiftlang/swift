// RUN: %swift -O -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK: define{{.*}}2fn
func fn() {
  println("two")
  println(0 - UInt(Process.arguments.count))
  println("three")
}
// All traps should be coalesced at the end.
// CHECK: ret
// CHECK-NOT: define
// CHECK: call void @llvm.trap(), !dbg ![[LOC:.*]]
// CHECK-NEXT: unreachable, !dbg ![[LOC]]
// CHECK: ![[LOC]] = !MDLocation(line: 0, scope
