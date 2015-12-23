// RUN: %target-swift-frontend -O -sil-inline-threshold 1 -emit-sil -primary-file %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol Pingable {
  typealias Tp
  func ping(x x : Tp) -> Tp

}

class Foo<T> : Pingable {
  func ping(x x : T) -> T { return x }
}

func main_func<T>(In In : T) {
  let x = Foo<T>()
  x.ping(x: In)
}

//CHECK: sil hidden @_TF21specialize_apply_conf11interestingFT_T_
//CHECK: function_ref @_TTSf4d___TTSg5Si___TF21specialize_apply_conf9main_funcurFT2Inx_T_
//CHECK-NEXT: apply
//CHECK: return
func interesting() {
  main_func(In: 3)
}
