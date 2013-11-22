// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
protocol AProtocol {
	func print()
}

class AClass : AProtocol {
	var x : Int
	init() { x = 0xDEADBEEF }
	func print() { println("x = \(x)")}
}
// CHECK: define void @_T17ProtocolContainer3fooFT1xPS_9AProtocol__T_
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[X:.*]] = alloca %P17ProtocolContainer9AProtocol_, align 8
// CHECK-NEXT: call void @llvm.dbg.declare(metadata !{%P17ProtocolContainer9AProtocol_* %[[X]]}, metadata ![[XMD:.*]])
// CHECK-NOT: "x"
// CHECK: ![[XMD]] = {{.*}}[ DW_TAG_arg_variable ] [x] [line [[@LINE+2]]]
// CHECK-NOT: "x"
func foo (x : AProtocol) {
	x.print() // Set breakpoint here
}
var aProtocol : AProtocol = AClass()
foo(aProtocol)
