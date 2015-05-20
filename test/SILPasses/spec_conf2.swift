// RUN: %target-swift-frontend -O -disable-arc-opts -sil-inline-threshold 0 -emit-sil -Xllvm -enable-destroyhoisting=false %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol P { func p() }
protocol Q { func q() }

class Foo: P, Q {
  func p() {}
  func q() {}
}

func inner_function<T : protocol<P, Q> >(In In : T) { }
func outer_function<T : protocol<P, Q> >(In In : T) { inner_function(In: In) }

//CHECK: sil shared @_TTSg5C10spec_conf23FooS0_S_1PS_S0_S_1QS____TF10spec_conf214outer_functionuRq_S_1Pq_S_1Q_FT2Inq__T_
//CHECK: function_ref @_TTSg5C10spec_conf23FooS0_S_1PS_S0_S_1QS____TF10spec_conf214inner_functionuRq_S_1Pq_S_1Q_FT2Inq__T_
//CHECK-NEXT: retain
//CHECK-NEXT: apply
//CHECK: return

outer_function(In: Foo())
