// RUN: %target-swift-frontend -primary-file %s -emit-ir -verify -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

protocol AProtocol {
  func f() -> String;
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}
class AnotherClass : AProtocol {
  func f() -> String { return "B" }
}

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction{{.*}}",{{.*}} elements: ![[PROTOS:[0-9]+]]
// CHECK-DAG: ![[PROTOS]] = !{![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = !DIDerivedType(tag: DW_TAG_inheritance,{{.*}} baseType: ![[PROTOCOL:"[^"]+"]]
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtMP12generic_args9AProtocol_",{{.*}} identifier: [[PROTOCOL]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "x", arg: 1,{{.*}} type: ![[T:.*]])
// CHECK-DAG: ![[T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction{{.*}}
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "y", arg: 2,{{.*}} type: ![[Q:.*]])
// CHECK-DAG: ![[Q]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq0_F12generic_args9aFunction{{.*}}
func aFunction<T : AProtocol, Q : AProtocol>(var x: T, var _ y: Q, _ z: String) {
   markUsed("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")

struct Wrapper<T: AProtocol> {

  init<U: AProtocol>(from : Wrapper<U>) {
  // CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Wrapper",{{.*}} identifier: "_TtGV12generic_args7WrapperQq_FS0_cu__Rq_S_9AProtocolqd__S1__FMGS0_q__FT4fromGS0_qd____GS0_q___")
    var wrapped = from
  }

  func passthrough(t: T) -> T {
    // CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "local",{{.*}} line: [[@LINE+1]],{{.*}} type: ![[LOCAL_T:[0-9]+]]
    var local = t
    // The type of local should have the context Wrapper<T>.
    // CHECK-DAG: ![[LOCAL_T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_V12generic_args7Wrapper"
    return local
  }
}

// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "f", {{.*}}, line: [[@LINE+1]], type: !"_TtFQq_F12generic_args5applyu0_rFTq_1fFq_q0__q0_Qq0_F12generic_args5applyu0_rFTq_1fFq_q0__q0_")
func apply<T, U> (x: T, f: (T)->(U)) -> U {
  return f(x)
}

