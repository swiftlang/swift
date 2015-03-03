// RUN: %target-swift-frontend -primary-file %s -emit-ir -verify -g -o - | FileCheck %s

protocol AProtocol {
  func f() -> String;
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}
class AnotherClass : AProtocol {
  func f() -> String { return "B" }
}


// CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction{{.*}}",{{.*}} elements: ![[PROTOS:[0-9]+]]
// CHECK-DAG: ![[PROTOS]] = !{![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = !MDDerivedType(tag: DW_TAG_inheritance,{{.*}} baseType: ![[PROTOCOL:"[^"]+"]]
// CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtMP12generic_args9AProtocol_",{{.*}} identifier: [[PROTOCOL]]
// CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "x", arg: 1,{{.*}} type: ![[T:.*]])
// CHECK-DAG: ![[T]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction{{.*}}
// CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "y", arg: 2,{{.*}} type: ![[Q:.*]])
// CHECK-DAG: ![[Q]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtQq0_F12generic_args9aFunction{{.*}}
func aFunction<T : AProtocol, Q : AProtocol>(var x: T, var y: Q, z: String) {
     println("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")

struct Wrapper<T: AProtocol> {

  init<U: AProtocol>(from : Wrapper<U>) {
  // CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "Wrapper",{{.*}} identifier: "_TtGV12generic_args7WrapperQq_FS0_cUS_9AProtocol__FMGS0_Q__US1___FT4fromGS0_Q___GS0_Qd____")
    var wrapped = from
  }

  func passthrough(t: T) -> T {
    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "local",{{.*}} line: [[@LINE+1]],{{.*}} type: ![[LOCAL_T:[0-9]+]]
    var local = t
    // The type of local should have the context Wrapper<T>.
    // CHECK-DAG: ![[LOCAL_T]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtQq_V12generic_args7Wrapper"
    return local
  }
}
