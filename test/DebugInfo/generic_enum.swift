// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
enum TrivialGeneric<T, U> {
  case x(T, U)
}

func unwrapTrivialGeneric<T, U>(tg:TrivialGeneric<T, U>) -> (T, U) {
  switch tg {
  case .x(var t, var u):
    return (t, u)
  }
}

func wrapTrivialGeneric<T, U>(t:T, u:U) -> TrivialGeneric<T, U> {
  // FIXME: full qualification required because of <rdar://problem/14994273>
  return TrivialGeneric<T, U>.x(t, u)
}
// FIXME: xCHECK below temporarily disabled
// CHECK: i32 [[@LINE+2]], metadata ![[TGT:.*]], i32 0{{.*}}[ DW_TAG_variable ] [tg] [line [[@LINE+2]]] [def]
// xCHECK: ![[TGT]] = {{.*}} [ DW_TAG_union_type ] [_TtGO13generic_union14TrivialGenericSiSS_]
var tg : TrivialGeneric<Int, String> = .x(23, "skidoo")
switch tg {
case .x(var t, var u):
  println("\(t) \(u)")
}
