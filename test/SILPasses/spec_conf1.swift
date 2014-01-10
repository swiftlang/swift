// RUN: %swift -O3 -sil-inline-threshold=0 -emit-sil %s | FileCheck %s
// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol P { func p() }
protocol Q { func q() }

class Foo: P, Q {
  func p() {}
  func q() {}
}

func inner_function<T : P>(In : T) { }
func outer_function<T : P>(In : T) { inner_function(In) }

//CHECK: sil private @_TF10spec_conf114outer_functionUS_1P__FT2InQ__T__spec0
//CHECK: function_ref @_TF10spec_conf114inner_functionUS_1P__FT2InQ__T__spec0
//CHECK-NEXT: apply
//CHECK: return

outer_function(Foo())
