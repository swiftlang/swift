// RUN: %swift -O3 -emit-sil %s | FileCheck %s
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

//CHECK: @_TF23devirt_archetype_method21interesting_code_hereFT_T_
//CHECK: function_ref @_TTWC23devirt_archetype_method3ABCS_8pingableS_FS1_4pingU_fRQPS1_FT_T__spec0
//CHECK: apply
//CHECK: return
func interesting_code_here() {
  var x = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(x)
}
