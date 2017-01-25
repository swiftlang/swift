// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

protocol AProtocol {
  func print()
}

class AClass : AProtocol {
   var x: UInt32
   init() { x = 0xDEADBEEF }
   func print() { markUsed("x = \(x)")}
}
// CHECK: define hidden void @_T017ProtocolContainer3foo{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[X:.*]] = alloca %P17ProtocolContainer9AProtocol_, align {{(4|8)}}
// CHECK:      call void @llvm.dbg.declare(metadata %P17ProtocolContainer9AProtocol_* %[[X]], metadata ![[XMD:.*]], metadata !{{[0-9]+}})
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
// CHECK-NOT: !DILocalVariable({{.*}} name: "x"
func foo (_ x : AProtocol) {
  var x = x
	x.print() // Set breakpoint here
}
var aProtocol : AProtocol = AClass()
foo(aProtocol)
