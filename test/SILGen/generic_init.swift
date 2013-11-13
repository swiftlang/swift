// RUN: %swift -emit-silgen %s | FileCheck %s

class Fff<T> { 
  class Bar {}; 
  static def f() -> Bar { return Bar() } 
}

// CHECK-LABEL: sil @_TCC12generic_init3Fff3BarCU__fMS1_FT_S1_ : $@thin <T> ((), Fff<T>.Bar.metatype) -> Fff<T>.Bar 
// CHECK: [[REF:%[0-9]+]] = function_ref @_TCC12generic_init3Fff3BarcU__fMS1_FT_S1_ : $@cc(method) @thin <T> ((), Fff<T>.Bar) -> Fff<T>.Bar
// CHECK-NEXT: [[SPEC_REF:%[0-9]+]] = apply [[REF]]<T = T>(%1) : $@cc(method) @thin <T> ((), Fff<T>.Bar) -> Fff<T>.Bar
// CHECK-NEXT: return