// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s -allow-deprecated-dag-overlap

func markUsed<T>(_ t: T) {}

enum TrivialGeneric<T, U> {
  case x(T, U)
}

func unwrapTrivialGeneric<T, U>(_ tg: TrivialGeneric<T, U>) -> (T, U) {
  switch tg {
  case .x(let t, let u):
    return (t, u)
  }
}

func wrapTrivialGeneric<T, U>(_ t: T, u: U) -> TrivialGeneric<T, U> {
  return .x(t, u)
}
// CHECK-DAG: ![[T1:.*]] = !DICompositeType({{.*}}name: "$s12generic_enum14TrivialGenericOys5Int64VSSGD"
// CHECK-DAG: ![[T1_MEMBER:.*]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[T1]]
// CHECK-DAG: ![[T1_ELTS:.*]] = !{![[T1_MEMBER]]}
// CHECK-DAG: ![[T1_CONTAINER:.*]] = !DICompositeType({{.*}}elements: ![[T1_ELTS]]
// CHECK-DAG: !DIGlobalVariable(name: "tg",{{.*}} line: [[@LINE+2]],{{.*}} type: ![[T1_CONTAINER]],{{.*}} isLocal: false, isDefinition: true
// CHECK-DAG: !DICompositeType({{.*}}, name: "$s12generic_enum14TrivialGenericOys5Int64VSSGD"
var tg : TrivialGeneric<Int64, String> = .x(23, "skidoo")
switch tg {
case .x(var t, var u):
  markUsed("\(t) \(u)")
}
