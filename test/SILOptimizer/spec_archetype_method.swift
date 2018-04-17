
// RUN: %target-swift-frontend -module-name spec_archetype_method -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -disable-arc-opts -emit-sil -primary-file %s | %FileCheck %s

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

//CHECK-LABEL: sil @$S21spec_archetype_method21interesting_code_hereyyF
//CHECK: function_ref @$S21spec_archetype_method12generic_call{{[_0-9a-zA-Z]*}}FAA3ABCC_Tg5
//CHECK-NEXT: apply
//CHECK:  function_ref @$S21spec_archetype_method6useFoo{{[_0-9a-zA-Z]*}}FAA3ABCC_Tg5 : $@convention(thin) (@guaranteed ABC) -> ()
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
