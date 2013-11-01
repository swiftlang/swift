// <rdar://problem/14826416>
// RUN: %swift -emit-silgen %s | FileCheck %s

func foo() -> Bool { return true }

func f<T, U>(t: T, u: U) {
  switch t {
  case is Int: println("Int")
  case is U: println("U")
  case _: println("other")
  }
}
// CHECK: sil @_T16switch_r148264161fU___FT1tQ_1uQ0__T_
// CHECK:   checked_cast_br archetype_to_concrete {{%.*}} : $*T to $*Int64, [[IS_INT:bb[0-9]+]], [[ISNT_INT:bb[0-9]+]]
// CHECK: [[ISNT_INT]]:
// CHECK:   checked_cast_br archetype_to_archetype {{%.*}} : $*T to $*U, [[ISNT_INT_IS_U:bb[0-9]+]], [[ISNT_INT_ISNT_U:bb[0-9]+]]
