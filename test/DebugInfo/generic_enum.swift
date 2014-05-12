// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
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
// CHECK: i32 [[@LINE+2]], metadata ![[TGT:.*]], i32 0{{.*}}[ DW_TAG_variable ] [tg] [line [[@LINE+2]]] [def]
// CHECK: ![[TGT]] = {{.*}} [ DW_TAG_union_type ] [_TtGO12generic_enum14TrivialGenericSiSS_]
var tg : TrivialGeneric<Int, String> = .x(23, "skidoo")
switch tg {
case .x(var t, var u):
  println("\(t) \(u)")
}
