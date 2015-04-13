// RUN: %target-swift-frontend -O -disable-arc-opts -sil-inline-threshold 0 -emit-sil -primary-file %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol pingable {
  func ping()
}

class ABC : pingable {
  func ping() {}
}

func generic_call<T : pingable>(#x: T) {
  x.ping()
}

struct A<B> : pingable {
  func ping() {}
}

func useFoo<T>(#x: T) {
  var a = A<T>()
  generic_call(x: a)
}

//CHECK-LABEL: sil hidden @_TF21spec_archetype_method21interesting_code_hereFT_T_
//CHECK: function_ref @_TTSg5C21spec_archetype_method3ABCS0_S_8pingableS____TF21spec_archetype_method12generic_callUS_8pingable__FT1xQ__T_
//CHECK-NEXT: retain
//CHECK-NEXT: apply
//CHECK:  function_ref @_TTSg5C21spec_archetype_method3ABC___TF21spec_archetype_method6useFooU__FT1xQ__T_ : $@convention(thin) (@in ABC) -> ()
//CHECK-NEXT: apply
//CHECK: return
func interesting_code_here() {
  var x = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(x: x)
  useFoo(x: x)
}
