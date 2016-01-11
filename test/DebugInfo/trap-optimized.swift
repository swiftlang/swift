// RUN: %target-swift-frontend -O -primary-file %s -emit-ir -g -o - | FileCheck %s
import StdlibUnittest
// CHECK-LABEL: define{{.*}}2fn
public var i : UInt32 = 1
public func fn() {
  _blackHole(i)
// CHECK-DAG: ![[LOC:.*]] = !DILocation(line: [[@LINE+1]], column: 16,
  _blackHole(0 - i)
// CHECK-DAG: ![[LOC2:.*]] = !DILocation(line: [[@LINE+1]], column: 16,
  _blackHole(0 - i)
  _blackHole(i)
}

// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC]]
// CHECK-DAG: unreachable, !dbg ![[LOC]]
// CHECK-DAG: call void @llvm.trap(), !dbg ![[LOC2]]
// CHECK-DAG: unreachable, !dbg ![[LOC2]]
