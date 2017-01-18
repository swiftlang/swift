// RUN: %target-swift-frontend -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol Pingable {
  associatedtype Tp
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
//CHECK-DAG: [[F:%[0-9]+]] = function_ref @{{.*}}_TTSg5Si___TF21specialize_apply_conf9main_funcurFT2Inx_T_{{.*}}scope
//CHECK-DAG: [[A:%[0-9]+]] = struct $Int
//CHECK-DAG: apply [[F]]([[A]])
//CHECK: return
func interesting() {
  main_func(In: 3)
}
