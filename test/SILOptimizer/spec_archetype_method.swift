// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass="Function Signature Optimization" -disable-arc-opts -emit-sil -primary-file %s | %FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol pingable {
  func ping()
}

class ABC : pingable {
  @inline(never)
  func ping() {}
}

@inline(never)
func generic_call<T : pingable>(x x: T) {
  x.ping()
}

struct A<B> : pingable {
  @inline(never)
  func ping() {}
}

@inline(never)
func useFoo<T>(x x: T) {
  let a = A<T>()
  generic_call(x: a)
}

//CHECK-LABEL: sil @_TF21spec_archetype_method21interesting_code_hereFT_T_
//CHECK: function_ref @_TTSg5C21spec_archetype_method3ABCS0_S_8pingableS____TF21spec_archetype_method12generic_call
//CHECK-NEXT: retain
//CHECK-NEXT: apply
//CHECK:  function_ref @_TTSg5C21spec_archetype_method3ABC___TF21spec_archetype_method6useFoo{{.*}} : $@convention(thin) (@owned ABC) -> ()
//CHECK-NEXT: apply
//CHECK: return
public
func interesting_code_here() {
  let x = ABC()
  // Make sure that we can specialize the function generic_call that has a
  // generic call to x.ping().
  generic_call(x: x)
  useFoo(x: x)
}
