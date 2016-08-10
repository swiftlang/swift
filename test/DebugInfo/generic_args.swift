// RUN: %target-swift-frontend -primary-file %s -emit-ir -verify -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

protocol AProtocol {
  func f() -> String
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}
class AnotherClass : AProtocol {
  func f() -> String { return "B" }
}

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction{{.*}}",{{.*}} elements: ![[PROTOS:[0-9]+]]
// CHECK-DAG: ![[PROTOS]] = !{![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = !DIDerivedType(tag: DW_TAG_inheritance,{{.*}} baseType: ![[PROTOCOL:[0-9]+]]
// CHECK-DAG: ![[PROTOCOL]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtMP12generic_args9AProtocol_",
// CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}} type: ![[T:.*]])
// CHECK-DAG: ![[T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_F12generic_args9aFunction
// CHECK-DAG: !DILocalVariable(name: "y", arg: 2,{{.*}} type: ![[Q:.*]])
// CHECK-DAG: ![[Q]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq0_F12generic_args9aFunction
func aFunction<T : AProtocol, Q : AProtocol>(_ x: T, _ y: Q, _ z: String) {
   markUsed("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")

struct Wrapper<T: AProtocol> {

  init<U: AProtocol>(from : Wrapper<U>) {
  // CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Wrapper",{{.*}} identifier: "_TtGV12generic_args7WrapperQq_FS0_cuRd__S_9AProtocolrFT4fromGS0_qd____GS0_x__")
    var wrapped = from
    wrapped = from
    _ = wrapped
  }

  func passthrough(_ t: T) -> T {
    // The type of local should have the context Wrapper<T>.
    // CHECK-DAG: ![[WRAPPER:.*]] = !DICompositeType({{.*}}identifier: "_TtQq_V12generic_args7Wrapper")
    // CHECK-DAG: !DILocalVariable(name: "local",{{.*}} line: [[@LINE+1]],{{.*}} type: ![[WRAPPER]]
    var local = t
    local = t
    return local
  }
}

// CHECK-DAG: ![[FNTY:.*]] = !DICompositeType({{.*}}identifier: "_TtFQq_F12generic_args5applyu0_rFTx1fFxq__q_Qq0_F12generic_args5applyu0_rFTx1fFxq__q_"
// CHECK-DAG: !DILocalVariable(name: "f", {{.*}}, line: [[@LINE+1]], type: ![[FNTY]])
func apply<T, U> (_ x: T, f: (T) -> (U)) -> U {
  return f(x)
}

