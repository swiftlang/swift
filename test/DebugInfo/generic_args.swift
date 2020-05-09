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

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sq_D",
// CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}} type: ![[LET_T:.*]])
// CHECK-DAG: ![[LET_T]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[T:.*]])
// CHECK-DAG: ![[T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxD"
// CHECK-DAG: !DILocalVariable(name: "y", arg: 2,{{.*}} type: ![[LET_Q:.*]])
// CHECK-DAG: ![[LET_Q]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[Q:.*]])
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
// CHECK-DAG: ![[LET_FNTY:.*]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[FNTY]])
// CHECK-DAG: !DILocalVariable(name: "f", {{.*}}, line: [[@LINE+1]], type: ![[LET_FNTY]])
func apply<T, U> (_ x: T, f: (T) -> (U)) -> U {
  return f(x)
}

