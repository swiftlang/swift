// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
protocol AProtocol {
	func f() -> String;
}
class AClass : AProtocol {
	func f() -> String {return "A"}
}

// CHECK:  call void @llvm.dbg.declare(metadata !{%swift.type* %{{.*}}}, metadata ![[TYPEARG:.*]]),
// CHECK: ![[TYPEARG]] = metadata !{i32 {{.*}}, metadata !{{.*}}, metadata !"$swift.type.x", metadata !{{.*}}, i32 [[@LINE+1]], metadata !44, i32 64, i32 0} ; [ DW_TAG_auto_variable ] [$swift.type.x] [line [[@LINE+1]]]
func aFunction <T : AProtocol> (x : T) {
	println("I am in aFunction: \(x.f())")
}

aFunction(AClass())
