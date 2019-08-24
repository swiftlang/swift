// RUN: %target-swift-frontend -O -sil-inline-threshold 0 -Xllvm -sil-print-debuginfo -emit-sil -primary-file %s | %FileCheck %s

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

//CHECK: sil hidden @$s21specialize_apply_conf11interestingyyF
//CHECK-DAG: [[F:%[0-9]+]] = function_ref @$s21specialize_apply_conf9main_func2Inyx_tlFSi_Tg5{{.*}}{{.*}}scope
//CHECK-DAG: [[A:%[0-9]+]] = struct $Int
//CHECK-DAG: apply [[F]]([[A]])
//CHECK: return
func interesting() {
  main_func(In: 3)
}
