// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

// CHECK: define{{.*}}@_T011autoclosure7call_meys5Int64VF
// CHECK-NOT: ret void
// CHECK: call void @llvm.dbg.declare{{.*}}, !dbg
// CHECK-NOT: ret void
// CHECK: , !dbg ![[DBG:.*]]
// CHECK: ret void

func get_truth(_ input: Int64) -> Int64 {
    return input % 2
}

// Since this is an autoclosure test, don't use &&, which is transparent.
infix operator &&&&& : LogicalConjunctionPrecedence

func &&&&&(lhs: Bool, rhs: @autoclosure () -> Bool) -> Bool {
  return lhs ? rhs() : false
}

func call_me(_ input: Int64) -> Void {
// rdar://problem/14627460
// An autoclosure should have a line number in the debug info and a scope line of 0.
// CHECK-DAG: !DISubprogram({{.*}}linkageName: "_T011autoclosure7call_meys5Int64VFSbyXKfu_",{{.*}} line: [[@LINE+3]],{{.*}} isLocal: false, isDefinition: true
// But not in the line table.
// CHECK-DAG: ![[DBG]] = !DILocation(line: [[@LINE+1]],
  if input != 0 &&&&& ( get_truth (input * 2 + 1) > 0 ) {
  }

}

call_me(5)
