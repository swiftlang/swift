// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
// CHECK: define linkonce_odr hidden void @_TFF11autoclosure7call_me
// CHECK: call void @llvm.dbg.value{{.*}}, !dbg
// CHECK: , !dbg ![[DBG:.*]]

func get_truth(input: Int) -> Int {
    return input % 2
}


// Since this is an autoclosure test, don't use &&, which is transparent.
operator infix &&&&& {
  associativity left
  precedence 120
}

func &&&&&(lhs: LogicValue, rhs: @auto_closure ()->LogicValue) -> Bool {
  return lhs.getLogicValue() ? rhs().getLogicValue() : false
}

func call_me(var input: Int) -> Void {
// rdar://problem/14627460
// An autoclosure should have a line number in the debug info and a scope line of 0.
// CHECK-DAG: "_TFF11autoclosure7call_me{{.*}}"{{.*}}[ DW_TAG_subprogram ] [line [[@LINE+3]]] [def] [scope 0]
// But not in the line table.
// CHECK-DAG: ![[DBG]] = metadata !{i32 [[@LINE+1]], i32
    if input != 0 &&&&& ( get_truth (input * 2 + 1) > 0 )
    {
        println ("Whew, passed that test.")
    }

}

call_me(5)
