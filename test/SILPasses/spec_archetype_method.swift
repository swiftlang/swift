// RUN: %swift -O3 -enable-arc-opts=false -sil-inline-threshold=0 -emit-sil %s | FileCheck %s
// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol pingable {
  func ping()
}

class ABC : pingable {
  func ping() {}
}

func generic_call<T : pingable>(x: T) {
  x.ping()
}

//CHECK: sil @_TF21spec_archetype_method21interesting_code_hereFT_T_
//CHECK: function_ref @_TF21spec_archetype_method12generic_callUS_8pingable__FT1xQ__T__spec0
//CHECK-NEXT: apply
//CHECK: return
func interesting_code_here() {
  var x = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(x)
}
