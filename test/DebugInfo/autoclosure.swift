// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK: define{{.*}}@_TFF11autoclosure7call_meFVs5Int64T_u_KT_Ps11BooleanType_
// CHECK-NOT: ret void
// CHECK: call void @llvm.dbg.declare{{.*}}, !dbg
// CHECK-NOT: ret void
// CHECK: , !dbg ![[DBG:.*]]
// CHECK: ret void

func get_truth(input: Int64) -> Int64 {
    return input % 2
}

// Since this is an autoclosure test, don't use &&, which is transparent.
infix operator &&&&& {
  associativity left
  precedence 120
}

func &&&&&(lhs: BooleanType, @autoclosure rhs: () -> BooleanType) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

func call_me(input: Int64) -> Void {
  var input = input
// rdar://problem/14627460
// An autoclosure should have a line number in the debug info and a scope line of 0.
// CHECK-DAG: !DISubprogram({{.*}}linkageName: "_TFF11autoclosure7call_meFVs5Int64T_u_KT_Ps11BooleanType_",{{.*}} line: [[@LINE+3]],{{.*}} isLocal: false, isDefinition: true
// But not in the line table.
// CHECK-DAG: ![[DBG]] = !DILocation(line: [[@LINE+1]],
  if input != 0 &&&&& ( get_truth (input * 2 + 1) > 0 ) {
  }

}

call_me(5)
