// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o -

func markUsed<T>(_ t: T) {}

protocol AProtocol {
  func print()
}

class AClass : AProtocol {
   var x: UInt32
   init() { x = 0xDEADBEEF }
   func print() { markUsed("x = \(x)")}
}
// CHECK: define hidden {{.*}}void @"$s17ProtocolContainer3foo{{[_0-9a-zA-Z]*}}F"
// CHECK-NEXT: entry:
// CHECK:      %[[X:.*]] = alloca %T17ProtocolContainer9AProtocolP, align {{(4|8)}}
// CHECK:      call void @llvm.dbg.declare(metadata %T17ProtocolContainer9AProtocolP* %[[X]], metadata ![[XMD:.*]], metadata !DIExpression())
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
func foo (_ x : AProtocol) {
  var x = x
	x.print() // Set breakpoint here
}
var aProtocol : AProtocol = AClass()
foo(aProtocol)
