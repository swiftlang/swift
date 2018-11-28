
// RUN: %target-swift-emit-silgen -module-name capture_typed_boxes -enable-sil-ownership %s | %FileCheck %s

func foo(_ x: Int) -> () -> Int {
  var x = x
  return { x }
}
// CHECK-LABEL: sil private @$s19capture_typed_boxes3fooySiycSiFSiycfU_ : $@convention(thin) (@guaranteed { var Int }) -> Int {
// CHECK:       bb0(%0 : @guaranteed ${ var Int }):

func closure(_ f: @escaping (Int) -> Int) -> Int {
  var f = f
  func bar(_ x: Int) -> Int {
    return f(x)
  }

  return bar(0)
}
// CHECK-LABEL: sil private @$s19capture_typed_boxes7closureyS3icF3barL_yS2iF : $@convention(thin) (Int, @guaranteed { var @callee_guaranteed (Int) -> Int }) -> Int {
// CHECK:       bb0(%0 : $Int, %1 : @guaranteed ${ var @callee_guaranteed (Int) -> Int }):

func closure_generic<T>(_ f: @escaping (T) -> T, x: T) -> T {
  var f = f
  func bar(_ x: T) -> T {
    return f(x)
  }

  return bar(x)
}
// CHECK-LABEL: sil private @$s19capture_typed_boxes15closure_generic{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @guaranteed <τ_0_0> { var @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0 } <T>) -> @out T {
// CHECK-LABEL: bb0(%0 : $*T, %1 : $*T, %2 : @guaranteed $<τ_0_0> { var @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0 } <T>):

