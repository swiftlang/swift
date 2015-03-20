// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

enum TrivialGeneric<T, U> {
  case x(T, U)
}

func unwrapTrivialGeneric<T, U>(tg: TrivialGeneric<T, U>) -> (T, U) {
  switch tg {
  case .x(var t, var u):
    return (t, u)
  }
}

func wrapTrivialGeneric<T, U>(t: T, u: U) -> TrivialGeneric<T, U> {
  return .x(t, u)
}
// CHECK-DAG: !MDGlobalVariable(name: "tg",{{.*}} line: [[@LINE+2]],{{.*}} type: !"_TtGO12generic_enum14TrivialGenericSiSS_",{{.*}} isLocal: false, isDefinition: true
// CHECK-DAG: !MDCompositeType(tag: DW_TAG_union_type, name: "TrivialGeneric", {{.*}}identifier: "_TtGO12generic_enum14TrivialGenericSiSS_"
var tg : TrivialGeneric<Int, String> = .x(23, "skidoo")
switch tg {
case .x(var t, var u):
  println("\(t) \(u)")
}
