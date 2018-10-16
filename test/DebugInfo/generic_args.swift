
// RUN: %target-swift-frontend -module-name generic_args -primary-file %s -emit-ir -verify -g -o - | %FileCheck %s -allow-deprecated-dag-overlap

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

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sq_D",{{.*}} elements: ![[PROTOS:[0-9]+]]
// CHECK-DAG: ![[PROTOS]] = !{![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = !DIDerivedType(tag: DW_TAG_inheritance,{{.*}} baseType: ![[PROTOCOL:[0-9]+]]
// CHECK-DAG: ![[PROTOCOL]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s12generic_args9AProtocol_pmD",
// CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}} type: ![[T:.*]])
// CHECK-DAG: ![[T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxD"
// CHECK-DAG: !DILocalVariable(name: "y", arg: 2,{{.*}} type: ![[Q:.*]])
// CHECK-DAG: ![[Q]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sq_D"
func aFunction<T : AProtocol, Q : AProtocol>(_ x: T, _ y: Q, _ z: String) {
   markUsed("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")

struct Wrapper<T: AProtocol> {

  init<U>(from : Wrapper<U>) {
  // CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Wrapper",{{.*}} identifier: "$s12generic_args7WrapperVyqd__GD")
    var wrapped = from
    wrapped = from
    _ = wrapped
  }

  func passthrough(_ t: T) -> T {
    // CHECK-DAG: ![[WRAPPER:.*]] = !DICompositeType({{.*}}identifier: "$s12generic_args7WrapperVyxGD")
    // CHECK-DAG: !DILocalVariable(name: "local",{{.*}} line: [[@LINE+1]],{{.*}} type: ![[T]]
    var local = t
    local = t
    return local
  }
}

// CHECK-DAG: ![[FNTY:.*]] = !DICompositeType({{.*}}identifier: "$sxq_Ignr_D"
// CHECK-DAG: !DILocalVariable(name: "f", {{.*}}, line: [[@LINE+1]], type: ![[FNTY]])
func apply<T, U> (_ x: T, f: (T) -> (U)) -> U {
  return f(x)
}

