// RUN: %swift -O3 -sil-inline-threshold 1 -emit-sil %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol Pingable {
  typealias Tp
  func ping(#x : Tp) -> Tp

}

class Foo<T> : Pingable {
  func ping(#x : T) -> T { return x }
}

func main_func<T>(#In : T) {
  var x = Foo<T>()
  x.ping(x: In)
}

//CHECK: sil @_TF21specialize_apply_conf11interestingFT_T_
//CHECK: function_ref @_TTSSi___TF21specialize_apply_conf9main_funcU__FT2InQ__T_
//CHECK-NEXT: apply
//CHECK: return
func interesting() {
  main_func(In: 3)
}
