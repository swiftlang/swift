// RUN: %swift -emit-silgen %s | FileCheck %s

class Fff<T> { 
  class Bar {}; 
  class func f() -> Bar { return Bar() }
}

// CHECK-LABEL: sil @_TFCC12generic_init3Fff3BarCU__fMS1_FT_S1_ : $@thin <T> (@thick Fff<T>.Bar.Type) -> @owned Fff<T>.Bar 
// CHECK: [[REF:%[0-9]+]] = function_ref @_TFCC12generic_init3Fff3BarcU__fMS1_FT_S1_ : $@cc(method) @thin <τ_0_0> (@owned Fff<τ_0_0>.Bar) -> @owned Fff<τ_0_0>.Bar
// CHECK-NEXT: [[SPEC_REF:%[0-9]+]] = apply [[REF]]<T>(%1) : $@cc(method) @thin <τ_0_0> (@owned Fff<τ_0_0>.Bar) -> @owned Fff<τ_0_0>.Bar
// CHECK-NEXT: return
