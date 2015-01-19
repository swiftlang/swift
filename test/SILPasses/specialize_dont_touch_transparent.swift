// RUN: %target-swift-frontend -disable-func-sig-opts -O -emit-sil -primary-file %s | FileCheck %s

// CHECK-LABEL: sil hidden [transparent] @_TF33specialize_dont_touch_transparent22transparentDoSomethingFT_VSs4Int8 : $@thin () -> Int8 {
// CHECK: bb0:
// CHECK: [[FUNC_REF:%[0-9]+]] = function_ref @_TF33specialize_dont_touch_transparent11doSomethingU__FQ_Q_ : $@thin <τ_0_0> (@out τ_0_0, @in τ_0_0) -> ()
// CHECK: apply [[FUNC_REF]]<Int8>(

func doSomething<T>(t : T) -> T {
  return t
}

@transparent
func transparentDoSomething() -> Int8 {
  var x : Int8 = 0
  return doSomething(x)
}
