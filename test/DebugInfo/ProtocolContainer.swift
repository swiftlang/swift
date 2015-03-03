// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

protocol AProtocol {
  func print()
}

class AClass : AProtocol {
   var x: UInt32
   init() { x = 0xDEADBEEF }
   func print() { println("x = \(x)")}
}
// CHECK: define hidden void @_TF17ProtocolContainer3foo
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[X:.*]] = alloca %P17ProtocolContainer9AProtocol_, align {{(4|8)}}
// CHECK:      call void @llvm.dbg.declare(metadata %P17ProtocolContainer9AProtocol_* %[[X]], metadata ![[XMD:.*]], metadata !{{[0-9]+}})
// CHECK-NOT: !MDLocalVariable({{.*}} name: "x"
// CHECK: ![[XMD]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "x", {{.*}}line: [[@LINE+2]]
// CHECK-NOT: !MDLocalVariable({{.*}} name: "x"
func foo (var x : AProtocol) {
	x.print() // Set breakpoint here
}
var aProtocol : AProtocol = AClass()
foo(aProtocol)
