// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

protocol AProtocol {
  func print()
}

class AClass : AProtocol {
   var x: UInt32
   init() { x = 0xDEADBEEF }
   func print() { markUsed("x = \(x)")}
}
// CHECK: define hidden void @_TF17ProtocolContainer3foo
// CHECK-NEXT: entry:
// CHECK: [[XADDR:[%].*]] = alloca %P17ProtocolContainer9AProtocol_*, align {{(4|8)}}
// CHECK: store %P17ProtocolContainer9AProtocol_* %0, %P17ProtocolContainer9AProtocol_** [[XADDR]], align {{(4|8)}}
// CHECK: call void @llvm.dbg.declare(metadata %P17ProtocolContainer9AProtocol_** [[XADDR]], metadata ![[XMD:.*]], metadata !{{[0-9]+}})
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
// CHECK: ![[XMD]] = !DILocalVariable(name: "x", arg: 1,{{.*}}line: [[@LINE+2]]
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
func foo (x : AProtocol) {
  x.print() // Set breakpoint here
}
var aProtocol : AProtocol = AClass()
foo(aProtocol)
