// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -verify -g -o - | FileCheck %s
protocol AProtocol {
	 func f() -> String;
}
class AClass : AProtocol {
      func f() -> String {return "A"}
}
class AnotherClass : AProtocol {
      func f() -> String {return "B"}
}

// CHECK-DAG: ![[T:.*]] ={{.*}} [ DW_TAG_structure_type ] [_TtQ_]
// CHECK-DAG: ![[Q:.*]] ={{.*}} [ DW_TAG_structure_type ] [_TtQ0_]
// CHECK-DAG: null, metadata ![[PROTOS:.*]], i32 0, null, null, null} ; [ DW_TAG_structure_type ] [_TtQ_]
// CHECK-DAG: ![[PROTOS]] = metadata !{metadata ![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = {{.*}}metadata ![[PROTOCOL:.*]]} ; [ DW_TAG_inheritance ]
// CHECK-DAG: ![[PROTOCOL]] = {{.*}}_TtP12generic_args9AProtocol_
// CHECK: metadata ![[T]],{{.*}}[ DW_TAG_arg_variable ] [x] [line [[@LINE+2]]]
// CHECK: metadata ![[Q]],{{.*}}[ DW_TAG_arg_variable ] [y] [line [[@LINE+1]]]
func aFunction <T : AProtocol, Q : AProtocol> (x : T, y : Q, z : String) {
     println("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")
